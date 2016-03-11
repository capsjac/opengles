{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}
module Main where
import Control.Applicative
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Time.Clock
import Graphics.OpenGLES
import qualified Graphics.UI.GLFW as GLFW
import System.Random
import Linear.Graphics

-- ghc examples/windmill.hs -lEGL -lGLESv2 -threaded && examples/windmill
main = do
	isOk <- GLFW.init
	when (not isOk) $ fail "Initialization Failed!"
	win <- initFullscreen 1600 1480 "Window Title"
	--win <- initWindow 600 480 "Window Title"
	       >>= maybe (fail "Failed to create a Window") return
	GLFW.setFramebufferSizeCallback win $ Just (const framesize)
	forkGL
		(GLFW.makeContextCurrent (Just win) >> return False)
		(GLFW.makeContextCurrent Nothing)
		(GLFW.swapBuffers win)
	forkIO $ mapM_ (putStrLn.("# "++)) =<< glLogContents
	future <- withGL $ mkWindmill >>= mkSomeObj
	obj <- expect future
	rand <- getStdGen
	t0 <- getCurrentTime
	let loop c = do
		withGL $ draw win rand t0 obj
		endFrameGL
		GLFW.pollEvents
		closing <- GLFW.windowShouldClose win
		when (not closing && c < 1000) $ loop (c + 1)
	loop 0

initWindow w h title = do
	GLFW.windowHint $ GLFW.WindowHint'Resizable True
	GLFW.createWindow w h title Nothing Nothing

initFullscreen w h title = do
	monitor <- GLFW.getPrimaryMonitor
	GLFW.createWindow w h title monitor Nothing

data Windmill = Windmill
	{ windmill :: Program Windmill
	, scaleRot :: Uniform Windmill Mat2
	, offset :: Uniform Windmill Vec2
	, pos :: Attrib Windmill Vec2
	, color :: Attrib Windmill Vec3
	} deriving Typeable

mkWindmill :: GL Windmill
mkWindmill = do
	Finished p <- glCompile NoFeedback
		[ vertexShader "windmill.vs" vsSrc
		, fragmentShader "windmill.fs" fsSrc ]
		$ \prog step msg bin ->
			putStrLn $ "> step " ++ show step ++ ", " ++ msg
	Windmill p <$> uniform "scaleRot" <*> uniform "offset"
		<*> attrib "pos" <*> attrib "color"

vsSrc = BS.pack $
	"#version 100\n\
	\uniform mat2 scaleRot;\
	\uniform vec2 offset;\
	\attribute vec2 pos;\
	\attribute vec3 color;\
	\varying vec4 vColor;\
	\void main() {\
	\    gl_Position = vec4(scaleRot * pos + offset, 0.0, 1.0);\
	\    vColor = vec4(color, 1.0);\
	\}"

fsSrc = BS.pack $
	"#version 100\n\
	\precision mediump float;\
	\varying vec4 vColor;\
	\void main() {\
	\    gl_FragColor = vColor;\
	\}"

data SomeObj = SomeObj
	{ prog :: Windmill
	, vao :: VertexArray Windmill
	, posBuf :: Buffer Vec2
	, colorBuf :: Buffer (V3 Word8)
	}

mkSomeObj :: Windmill -> GL SomeObj
mkSomeObj prog@Windmill{..} = do
	posBuf <- glLoad app2gl quadPos
	colorBuf <- glLoad app2gl quadCol
	vao <- glVA [ pos &= posBuf, normalized color &= colorBuf ]
	return SomeObj {..}

-- Square with diagonal < 2 so that it fits in a [-1,1] square
-- regardless of rotation.
quadPos :: [Vec2]
quadPos = [V2 (-0.7) (-0.7), V2 0.7 (-0.7), V2 (-0.7) 0.7, V2 0.7 0.7]

quadCol :: [V3 Word8]
quadCol = [V3 0x00 0xFF 0x00, V3 0x00 0x00 0xFF, V3 0xFF 0x00 0x00, V3 0x30 0x7F 0xFF]

draw :: GLFW.Window -> StdGen -> UTCTime -> SomeObj -> GL ()
draw win seed t0 SomeObj{..} = do
	(w, h) <- GLFW.getWindowSize win
	let (wx, hx) = (16.0, realToFrac $ h `div` (w `div` 16))
	t <- getCurrentTime
	let time = realToFrac $ diffUTCTime t t0
	let angles = [angl * time | angl <- rollDices seed]
	clear [ clearColor 0.2 0.2 0.3 1.0 ] colorBuffer
	let Windmill{..} = prog
	forM_ [(i, j) | i <- [0..wx-1], j <- [0..hx-1]] $ \(i, j) -> do
		let a = angles !! floor (i + wx * j)
		glDraw triangleStrip windmill []
			[ scaleRot $= cos a *!! scale2 (1/wx) (1/hx) !*! rotate2 a
			, offset $= V2 (2/wx*(0.5+i)-1) (2/hx*(0.5+j)-1) ]
			vao $ takeFrom 0 4

rollDices :: StdGen -> [Float]
rollDices = randomRs (0.1, 2*3.14)
