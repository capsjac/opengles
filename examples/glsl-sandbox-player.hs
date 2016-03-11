{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where
import Control.Applicative
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Time.Clock
import Graphics.OpenGLES
import qualified Graphics.UI.GLFW as GLFW

-- ghc examples/glsl-sandbox-player.hs -lEGL -lGLESv2 -threaded && examples/glsl-sandbox-player < examples/glsl-17617.fs
main :: IO ()
main = do
	putStrLn "Usage: Paste any pixel shader from https://glsl.heroku.com/ and press Ctrl-D\n(In other words, read from stdin.)"
	shaderSource <- getContents
	putStrLn shaderSource
	isOk <- GLFW.init
	if not isOk then (fail "Initialization Failed!") else return ()
	(win, w, h) <- initFullscreen "GLSL Sandbox Player"
	--(win, w, h) <- initWindow 600 480 "Window Title"
	forkGL (GLFW.makeContextCurrent (Just win) >> return False)
		(GLFW.makeContextCurrent Nothing)
		(GLFW.swapBuffers win)
	--GLFW.swapInterval 1
	forkIO $ mapM_ (putStrLn.("# "++)) =<< glLogContents
	future <- withGL $ mkPlayer (B.pack shaderSource) >>= mkPlayerObj
	obj <- expect future
	t0 <- getCurrentTime
	let loop c = do
		withGL $ draw win t0 (realToFrac w) (realToFrac h) obj
		endFrameGL
		putStrLn . show $ c
		GLFW.pollEvents
		closing <- GLFW.windowShouldClose win
		when (not closing && c < 1000) $ loop (c+1)
	loop 0
	GLFW.destroyWindow win
	GLFW.terminate
	--(w, h) <- GLFW.getFramebufferSize win -- XXX returns smaller size

initWindow w h title = do
	GLFW.windowHint $ GLFW.WindowHint'Resizable True
	win <- GLFW.createWindow w h title Nothing Nothing
		>>= maybe (fail "Failed to create a Window") return
	return (win, w, h)

initFullscreen title = do
	Just monitor <- GLFW.getPrimaryMonitor
	Just (GLFW.VideoMode w h _ _ _ _) <- GLFW.getVideoMode monitor
	--(w, h) <- GLFW.getMonitorPhysicalSize monitor -- XXX returns (0,0)
	win <- GLFW.createWindow w h title (Just monitor) Nothing
	       >>= maybe (fail "Failed to create a Window") return
	return (win, w, h)

data Player = Player
	{ player :: Program Player
	, p_time :: Uniform Player Float
	, p_mouse :: Uniform Player Vec2
	, p_resolution :: Uniform Player Vec2
	, p_backbuffer :: Uniform Player Int32
	, p_surfaceSize :: Uniform Player Vec2
	, p_pos :: Attrib Player Vec2
	} deriving Typeable

mkPlayer :: B.ByteString -> GL Player
mkPlayer fsSrc = do
	Finished p <- glCompile NoFeedback
		[ vertexShader "glsl-sandbox.vs" vsSrc
		, fragmentShader "custom-fragment-shader" fsSrc ]
		$ \prog step msg bin ->
			putStrLn $ "> step " ++ show step ++ ", " ++ msg
	Player p <$> uniform "time" <*> uniform "mouse"
		<*> uniform "resolution" <*> uniform "backbuffer"
		<*> uniform "surfaceSize" <*> attrib "pos"

vsSrc = B.pack $
	"attribute vec2 pos;\n" ++
	"varying vec2 surfacePosition;\n" ++
	"void main() {\n" ++
	"    surfacePosition = vec2(0, 0);\n" ++
	"    gl_Position = vec4(pos, 0, 1);\n" ++
	"}\n"

data PlayerObj = PlayerObj
	{ prog :: Player
	, vao :: VertexArray Player
	, posBuf :: Buffer Vec2
	}

mkPlayerObj :: Player -> GL PlayerObj
mkPlayerObj prog@Player{..} = do
	posBuf <- glLoad app2gl posData
	vao <- glVA [ p_pos &= posBuf ]
	return PlayerObj {..}

posData = [V2 (-1) 1, V2 1 1, V2 (-1) (-1), V2 1 (-1)]

draw :: GLFW.Window -> UTCTime -> Float -> Float -> PlayerObj -> GL ()
draw win t0 w h PlayerObj{..} = do
	clear [{-clearColor 0.2 0.2 0.3 1.0-}] colorBuffer
	t <- getCurrentTime
	let time = realToFrac $ diffUTCTime t t0
	(x, y) <- GLFW.getCursorPos win
	let Player{..} = prog
	result <- glDraw triangleStrip player
		[  ] --[ begin culling, cullFace hideBack]
		[ p_time $= time, p_mouse $= V2 (re x/w) (re y/h), p_resolution $= V2 w h, p_backbuffer $= 0, p_surfaceSize $= V2 w h ]
		vao $ takeFrom 0 4
	putStrLn . show $ result
	where re = realToFrac :: Double -> Float
