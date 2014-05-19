-- | $ ghc example -lEGL && ./example
module Main where
import Control.Concurrent
--import Graphics.EGL
import Graphics.OpenGLES
import qualified Graphics.UI.GLFW as GLFW
import Foreign.Marshal

main :: IO ()
main = do
	let title = "Window Title"
	    ww = 600
	    wh = 480
	    fullsc = False
	isOk <- GLFW.init
	if not isOk then (fail "Initialization Failed!") else return ()
	fullScreen <- GLFW.getPrimaryMonitor
	let mon = if fullsc then fullScreen else Nothing
	GLFW.windowHint $ GLFW.WindowHint'Resizable True
	--GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'OpenGLES
	win <- GLFW.createWindow ww wh title mon Nothing
		   >>= maybe (fail "Failed to create a Window") return
	GLFW.makeContextCurrent (Just win)
	GLFW.swapInterval 1
	GLFW.setFramebufferSizeCallback win $ Just $ \_ w h -> return ()
	exts <- getGLExtensions
	putStrLn $ show exts
	putStrLn "Main."
	putStrLn =<< getGLVersion
	pi <- setupGraphics
	putStrLn "ready."
	glClearColor 1 1 1 1
	gameloop win pi
	
	GLFW.destroyWindow win
	GLFW.terminate

beginFrame :: IO ()
beginFrame = do
	clearBuffer True True False

endFrame :: GLFW.Window -> IO ()
endFrame win = do
	GLFW.swapBuffers win
	GLFW.pollEvents
	threadDelay $ floor $ 1000000 * 0.01

gameloop :: GLFW.Window -> Context -> IO ()
gameloop win opt = do
	beginFrame
	drawFrame win opt
	endFrame win
	shouldExit <- GLFW.windowShouldClose win
	if shouldExit
		then return ()
		else gameloop win opt

drawFrame :: GLFW.Window -> Context -> IO ()
drawFrame win (program,vposHandle) = do
	glLineWidth 5.0
	
	useProgram program
	showError "useProgram"
	
	withArray (map realToFrac gTriangleVertices) $ \v->
		vertexAttribPointer vposHandle 2 FloatT False 0 v
	showError "glVertexAttribPointer"
	enableVertexAttribArray vposHandle
	showError "glEnableVertexAttribArray"
	drawArrays Triangles 0 3
	showError "glDrawArrays"

gVertexShader =
	"attribute vec4 vPosition;\n" ++
	"void main() {\n" ++
	"  gl_Position = vPosition;\n" ++
	"}\n"

gFragmentShader =
	"//precision mediump float;\n" ++
	"void main() {\n" ++
	"  gl_FragColor = vec4(0.0, 1.0, 0.0, 1.0);\n" ++
	"}\n"

type Context = (Program, Int)
setupGraphics :: IO Context
setupGraphics = do
	maybeProgram <- createProgram
		[("vert", gVertexShader)]
		[("frag", gFragmentShader)]
	case maybeProgram of
		Left msg -> error $ "Could not create program!\n" ++ concat msg
		Right program -> do
			gvPositionHandle <- getAttribLocation program "vPosition"
			showError "glGetAttribLocation"
			glViewport 0 0 600 480
			showError "glViewport"
			return (program, gvPositionHandle)

gTriangleVertices = [0.0, 0.5,-0.5,-0.5, 0.5,-0.5] :: [Float]
