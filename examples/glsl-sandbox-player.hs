{-# LANGUAGE RecordWildCards #-}
module Main where
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Time.Clock
import Graphics.OpenGLES
import Graphics.OpenGLES.Utils
import qualified Graphics.UI.GLFW as GLFW

-- ghc -lEGL -O2 glsl-sandbox-player
main :: IO ()
main = do
	putStrLn "Usage: Paste any shader from https://glsl.heroku.com/ and press Ctrl-D\n(In other words, read from stdin.)"
	shaderSource <- getContents
	putStrLn $ show $ length shaderSource
	isOk <- GLFW.init
	if not isOk then (fail "Initialization Failed!") else return ()
	(win, w, h) <- initFullscreen "Window Title"
	--(win, w, h) <- initWindow 600 480 "Window Title"
	GLFW.makeContextCurrent (Just win)
	--GLFW.swapInterval 1
	--(w, h) <- GLFW.getFramebufferSize win -- XXX returns smaller size
	
	putStrLn $ show (w, h)
	glViewport 0 0 (fromIntegral w) (fromIntegral h)
	glClearColor 0.2 0.2 0.3 1.0
	
	glm <- initGLManager
	startedAt <- getCurrentTime
	Right call <- compileCall glm (quad (BS.pack shaderSource))
	putStrLn $ show call
	let cxt = Context win call startedAt 0 w h
	mainloop cxt
	
	GLFW.destroyWindow win
	GLFW.terminate

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
	

data Context = Context
	{ win :: GLFW.Window
	, call :: DrawCall
	, startedAt :: UTCTime
	, loopcount :: Int
	, ww :: Int
	, wh :: Int
	}

quadPos :: [Float]
quadPos = [-1,-1, 1,-1, -1,1, 1, 1.0]

quad :: BS.ByteString -> DrawCall
quad customShader = DrawUnit
	TriangleStrip
	(Program "glsl-sandbox"
		[ VertexShader "pure" vertexShader
		, FragmentShader "custom-fragment-shader" customShader ])
	(DrawConfig True True False False)
	[ UniformVar "time" (Uniform1f 0)
	, UniformVar "mouse" (Uniform2f (Vec2 0 0))
	, UniformVar "resolution" (Uniform2f (Vec2 0 0))
	, UniformVar "backbuffer" (Uniform1i 0)
	, UniformVar "surfaceSize" (Uniform2f (Vec2 0 0))
	]
	[ Vertex "pos" (FloatV 2 quadPos) ]
	[]
	(VFromCount 0 4)

replaceUniforms old [] = old
replaceUniforms (old@(UniformVar name val ref):xs) new =
	loop new : replaceUniforms xs new
	where
		loop ((name', val'):ys) | name' == name = UniformVar name val' ref
		loop ((name', val'):ys) = loop ys
		loop [] = old
replaceUniforms [] new = []

params old time mouse res = replaceUniforms old $
	[ ("time", Uniform1f time)
	, ("mouse", Uniform2f mouse)
	, ("resolution", Uniform2f res)
	, ("backbuffer", Uniform1i 0)
	, ("surfaceSize", Uniform2f res)
	]

vertexShader = BS.pack $
	"attribute vec2 pos;\n" ++
	"varying vec2 surfacePosition;\n" ++
	"void main() {\n" ++
	"    surfacePosition = vec2(0, 0);\n" ++
	"    gl_Position = vec4(pos, 0, 1);\n" ++
	"}\n"

mainloop :: Context -> IO ()
mainloop cxt@Context{..} = do
	-- beginFrame
	--disable Dither
	--putStr "start " >> getCurrentTime >>= putStrLn . show
	glClear 0x4000
	--putStr "clear " >> getCurrentTime >>= putStrLn . show	
	draw cxt
	--putStr "drawn " >> getCurrentTime >>= putStrLn . show
	
	-- endFrame
	GLFW.swapBuffers win
	--putStr "swapd " >> getCurrentTime >>= putStrLn . show
	GLFW.pollEvents

	shouldExit <- GLFW.windowShouldClose win
	if shouldExit || loopcount > 1000
		then return ()
		else mainloop cxt { loopcount = loopcount + 1 }

draw (Context win call startedAt _ w h) = do
	time <- getCurrentTime
	putStrLn $ show time
	let t = realToFrac $ diffUTCTime time startedAt
	-- modify call here
	let DrawCall m b c p e f g = call
	(x, y) <- GLFW.getCursorPos win
	let unif = params p t (Vec2 (re x/float w) (re y/float h)) (Vec2 (float w) (float h))
	drawData $ DrawCall m b c unif e f g

float :: Int -> Float
float = fromIntegral

re :: Double -> Float
re = realToFrac
