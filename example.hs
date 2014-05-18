module Main where
import Control.Concurrent
--import Graphics.EGL
import Graphics.OpenGLES
import qualified Graphics.UI.GLFW as GLFW
import Foreign.Marshal

main :: IO ()
main = do
	let title = "Window Title"
	    ww = 1600
	    wh = 1480
	    fullsc = True
	isOk <- GLFW.init
	if not isOk then (fail "Initialization Failed!") else return ()
	fullScreen <- GLFW.getPrimaryMonitor
	let mon = if fullsc then fullScreen else Nothing
	GLFW.windowHint $ GLFW.WindowHint'Resizable True
	win <- GLFW.createWindow ww wh title mon Nothing
		   >>= maybe (fail "Failed to create a Window") return
	GLFW.makeContextCurrent (Just win)
	GLFW.swapInterval 1
	GLFW.setFramebufferSizeCallback win $ Just $ \_ w h -> return ()
	--aaa <- getGLExtensions
	--putStrLn $ show aaa
	putStrLn "Main."
	pi <- setupGraphics
	putStrLn "ready."
	glClearColor 1 1 1 1
	gameloop win pi
	
	GLFW.destroyWindow win
	GLFW.terminate

beginFrame :: IO ()
beginFrame = do
	--Box (V2 wl wt) (V2 wr wb) <- fmap realToFrac <$> readIORef (refRegion sys)
	--glViewport 0 0 (floor $ wr - wl) (floor $ wb - wt)
	--GL.matrixMode $= GL.Projection
	--glLoadIdentity
	--GL.ortho wl wr wb wt 0 (-100)
	--GL.matrixMode $= GL.Modelview 0
	clearBuffer True True False

endFrame :: GLFW.Window -> IO ()
endFrame win = do
	GLFW.swapBuffers win
	GLFW.pollEvents
	threadDelay $ floor $ 1000000 * 0.01
	--Just t <- GLFW.getTime
	--n <- readIORef (refFrameCounter sys)
	--fps <- readIORef (theFPS sys)
	--threadDelay $ max 0 $ floor $ (1000000 *) $ fromIntegral n / fromIntegral fps - t
	--if t > 1
	--	then GLFW.setTime 0 >> writeIORef (currentFPS sys) n >> writeIORef (refFrameCounter sys) 0
	--	else writeIORef (refFrameCounter sys) (succ n)

--gameloop :: GLFW.Window -> IO ()
gameloop win opt = do
	beginFrame
	drawFrame win opt
	endFrame win
	shouldExit <- GLFW.windowShouldClose win
	if shouldExit
		then return ()
		else gameloop win opt

--drawFrame :: GLFW.Window -> IO ()
drawFrame win (program,vposHandle) = do
	glLineWidth 5.0

	--[buf] <- genObjects 1
	--deleteObjects [buf :: Buffer]
	
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

setupGraphics :: IO (Program, Int)
setupGraphics = do
	maybeProgram <- createProgram gVertexShader gFragmentShader
	case maybeProgram of
		Nothing -> error "Could not create program!"
		Just program -> do
			gvPositionHandle <- getAttribLocation program "vPosition"
			showError "glGetAttribLocation"
			glViewport 0 0 600 480
			showError "glViewport"
			return (program, gvPositionHandle)

gTriangleVertices = [0.0, 0.5,-0.5,-0.5, 0.5,-0.5] :: [Float]
