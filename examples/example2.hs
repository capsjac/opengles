{-# LANGUAGE RecordWildCards #-}
module Main where
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Time.Clock
import Graphics.OpenGLES
import Graphics.OpenGLES.Utils
import qualified Graphics.UI.GLFW as GLFW
import System.Random

-- ghc -lEGL -O2 example2
main :: IO ()
main = do
	isOk <- GLFW.init
	if not isOk then (fail "Initialization Failed!") else return ()
	win <- initFullscreen 1600 1480 "Window Title"
	--win <- initWindow 600 480 "Window Title"
	       >>= maybe (fail "Failed to create a Window") return
	GLFW.makeContextCurrent (Just win)
	--GLFW.swapInterval 1
	(w, h) <- GLFW.getWindowSize win
	
	--disable Dither
	glViewport 0 0 (fromIntegral w) (fromIntegral h)
	glClearColor 0.2 0.2 0.3 1.0
	
	rand <- getStdGen
	glm <- initGLManager
	startedAt <- getCurrentTime
	Right call <- compileCall glm quad
	let cxt = Context win rand call startedAt 0
	mainloop cxt
	info <- sequence [getGLVendor, getGLRenderer, getGLVersion, getGLShadingLanguageVersion]
	putStrLn . show $ info
	exts <- getGLExtensions
	putStrLn . show $ exts
	GLFW.destroyWindow win
	GLFW.terminate

initWindow w h title = do
	GLFW.windowHint $ GLFW.WindowHint'Resizable True
	GLFW.createWindow w h title Nothing Nothing

initFullscreen w h title = do
	monitor <- GLFW.getPrimaryMonitor
	GLFW.createWindow w h title monitor Nothing

data Context = Context
	{ win :: GLFW.Window
	, seed :: StdGen
	, call :: DrawCall
	, startedAt :: UTCTime
	, loopcount :: Int
	}

-- Square with diagonal < 2 so that it fits in a [-1..1]^2 square
-- regardless of rotation.
quadPos :: [Float]
quadPos = [-0.7,-0.7, 0.7,-0.7, -0.7,0.7, 0.7, 0.7]

--quadCol :: [Word8]
quadCol = [0x00,0xFF,0x00, 0x00,0x00,0xFF, 0xFF,0x00,0x00, 0x30,0x7F,0xFF]

quad :: DrawCall
quad = DrawUnit
	TriangleStrip
	(Program "es2es3"
		[ VertexShader "test.vs" vertexShader
		, FragmentShader "test.fs" fragmentShader ])
	[]--(DrawConfig True True False False)
	[ UniformVar "scaleRot" $ UniformMat2 (structMat2 [1,0,0,1])
	, UniformVar "offset" $ Uniform2f (Vec2 0 0) ]
	[ Vertex "pos" (FloatV 2 quadPos)
	, NormalizedVertex "color" (UByteV 3 quadCol) ]
	--[]
	(VFromCount 0 4)

vertexShader = BS.pack $
	"#version 100\n" ++
	"uniform mat2 scaleRot;\n" ++
	"uniform vec2 offset;\n" ++
	"attribute vec2 pos;\n" ++
	"attribute vec4 color;\n" ++
	"varying vec4 vColor;\n" ++
	"void main() {\n" ++
	"    gl_Position = vec4(scaleRot*pos + offset, 0.0, 1.0);\n" ++
	"    vColor = color;\n" ++
	"}\n"

fragmentShader = BS.pack $
	"#version 100\n" ++
	"precision mediump float;\n" ++
	"varying vec4 vColor;\n" ++
	"void main() {\n" ++
	"    gl_FragColor = vColor;\n" ++
	"}\n"

mainloop :: Context -> IO ()
mainloop cxt@Context{..} = do
	-- beginFrame
	GLFW.pollEvents
	
	--putStr "start " >> getCurrentTime >>= putStrLn . show
	glClear 0x4000
	--putStr "clear " >> getCurrentTime >>= putStrLn . show
	es2draw cxt
	--putStr "drawn " >> getCurrentTime >>= putStrLn . show
	GLFW.swapBuffers win
	--putStr "swapd " >> getCurrentTime >>= putStrLn . show
	-- endFrame
	
	--threadDelay 16667
	shouldExit <- GLFW.windowShouldClose win
	if shouldExit || loopcount > 1000
		then return ()
		else mainloop cxt { loopcount = loopcount + 1 }

es2draw (Context win seed call startedAt _) = do
	(w, h) <- GLFW.getWindowSize win
	let (wx, hx) = (16, h `div` (w `div` 16))
	let elems = wx * hx
	let angleVelo = take elems $ rollDices seed
	time <- getCurrentTime
	putStrLn $ show time
	let t = realToFrac $ diffUTCTime time startedAt
	let angles = [e * t * 100 | e <- angleVelo]
	let (r_x, r_y) = (1 / float wx, 1 / float hx)
	forM_ [0..wx*hx-1] $ \i -> do
		let a = angles !! i
		-- modify call here
		let DrawCall m b c [UniformVar _ _ h, UniformVar _ _ j] e f = call
		let unif =
			[ UniformVar "scaleRot" (UniformMat2 $
				structMat2 [r_x,0,0,r_y] .*. rotMatrix2 a) h
			, UniformVar "offset" (Uniform2f $
				Vec2 (2/float wx*(0.5+float(i`mod`wx))-1) (2/float hx*(0.5+float(i`div`wx))-1)) j
			]
		--putStr "6 " >> getCurrentTime >>= putStrLn . show
		drawData $ DrawCall m b c unif e f
		--putStr "9 " >> getCurrentTime >>= putStrLn . show

float :: Int -> Float
float = fromIntegral

rollDices :: StdGen -> [Float]
rollDices = randomRs (0, 2*3.14*0.01)
