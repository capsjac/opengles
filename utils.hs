module Main where
import Control.Concurrent (threadDelay)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import Data.Word (Word8)
import Graphics.OpenGLES
import qualified Graphics.UI.GLFW as GLFW
import Data.Array.ST (newArray, getElems, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import GHC.ST (runST, ST)
data Context = Context
	deriving Show

main :: IO ()
main = do
	let title = "Window Title"
	let ww = 1600
	let wh = 1480
	let fullsc = True
	isOk <- GLFW.init
	if not isOk then (fail "Initialization Failed!") else return ()
	fullScreen <- GLFW.getPrimaryMonitor
	let mon = if fullsc then fullScreen else Nothing
	GLFW.windowHint $ GLFW.WindowHint'Resizable True
	win <- GLFW.createWindow ww wh title mon Nothing
		   >>= maybe (fail "Failed to create a Window") return
	GLFW.makeContextCurrent (Just win)
	--GLFW.swapInterval 1
	GLFW.setFramebufferSizeCallback win $ Just resizeCallback
	wdim@(w,h) <- GLFW.getWindowSize win

	cont <- return Context
	glViewport 0 0 (fromIntegral w) (fromIntegral h)
	glClearColor 0.5 1 1 1
	
	mainloop win cont
	
	GLFW.destroyWindow win
	GLFW.terminate

resizeCallback _ w h = return ()

mainloop win cont = do
	glClear 0x4000
	glm <- initGLManager
	putStrLn "### "
	call <- compileCall glm awesomeObject
	putStrLn $ show call
	putStrLn "==="
	case call of Right ok -> drawData ok
	putStrLn "### All OK"

	GLFW.swapBuffers win
	GLFW.pollEvents
	threadDelay $ 1000000

	shouldExit <- GLFW.windowShouldClose win
	if shouldExit
		then return ()
		else mainloop win cont


data BufferMarkup = BufferMarkup [String] [String]

packToBlob :: BufferMarkup -> Blob
packToBlob (BufferMarkup types values) =
	Blob $ B.concat $ zipWith toBS (cycle types) values

toBS :: String -> String -> B.ByteString
toBS "float" v = B.pack $ cast (read v)
toBS "hex" v = B.pack $ map (read.("0x"++)) (chunkOf v)

chunkOf [] = []
chunkOf (a:b:xs) = [a, b] : chunkOf xs

{-# INLINE cast #-}
cast :: Float -> [Word8]
cast x = runST (newArray (0 :: Int, 3) x >>= castSTUArray >>= getElems)

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

packedBlob = packToBlob vertexColors

vertexColors = BufferMarkup
	["float", "float", "hex"]
	["-0.7", "-0.7", "00FF00"
	, "0.7", "-0.7", "0000FF"
	,"-0.7",  "0.7", "FF0000"
	, "0.7",  "0.7", "00FFFF"
	]
