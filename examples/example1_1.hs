{-# LANGUAGE RecordWildCards #-}
-- ghc -lEGL example2.hs  && ./example2
module Main where
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8 as BS
import Graphics.OpenGLES
import Graphics.OpenGLES.Utils
import qualified Graphics.UI.GLFW as GLFW


data Context = Context
	{ win :: GLFW.Window
	, glm :: GLManager
	, call :: DrawCall
	} deriving Show

main :: IO ()
main = do
	GLFW.init
	monitor <- GLFW.getPrimaryMonitor
	let title = "Example 2"
	Just win <- GLFW.createWindow 0 0 title monitor Nothing
	GLFW.makeContextCurrent (Just win)
	(w, h) <- GLFW.getWindowSize win
	--GLFW.swapInterval 1
	
	glm <- initGLManager
	let cxt = Context win glm
	
	glViewport 0 0 (fromIntegral w) (fromIntegral h)
	glClearColor 0.5 1 1 1
	
	putStrLn "### Start Compile"
	Right call <- compileCall glm awesomeObject
	putStrLn $ show call
	
	mainloop (cxt call)

	GLFW.destroyWindow win
	GLFW.terminate

mainloop cxt@Context{..} = do
	glClear 0x4000
	putStrLn "=== Rendering"
	drawData call
	putStrLn "### All OK"

	GLFW.swapBuffers win
	GLFW.pollEvents
	threadDelay 16667 -- 1/60 sec

	shouldExit <- GLFW.windowShouldClose win
	if shouldExit
		then putStrLn "See you!"
		else mainloop cxt

awesomeObject =
	DrawUnit
		TriangleStrip
		(Program "test"
			[ VertexShader "test.vs" vertexShader2
			, FragmentShader "test.fs" fragmentShader2 ])
		(DrawConfig True True False False)
		[ UniformVar "scaleRot" $ UniformMat2 (structMat2 [1,0,0,1])
		, UniformVar "offset" $ Uniform2f (Vec2 0 0)
		]
		[ Vertex "pos" (FloatV 2 [-0.7,-0.7,0.7,-0.7,-0.7,0.7,0.7,0.7])
		, NormalizedVertex "color" (UByteV 3 [0,255,0, 0,0,255, 255,0,0, 0,255,255])
		]
		[]
		(VFromCount 0 4)

vertexShader2 = BS.pack $
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

fragmentShader2 = BS.pack $
	"#version 100\n" ++
	"precision mediump float;\n" ++
	"varying vec4 vColor;\n" ++
	"void main() {\n" ++
	"    gl_FragColor = vColor;\n" ++
	"}\n"

