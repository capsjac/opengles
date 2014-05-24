module Main where
import Control.Concurrent (threadDelay)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import Data.Word (Word8)
import Graphics.OpenGLES hiding (UByteT,FloatT)
import Graphics.OpenGLES.Types
import Graphics.OpenGLES.Utils
import qualified Graphics.UI.GLFW as GLFW
import Data.Array.ST (newArray, getElems, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import GHC.ST (runST, ST)

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
	--putStrLn =<< getGLVersion
	--putStrLn $ show detectGLESVersion

	--cont <- es2init wdim
	--putStrLn "ready."
	viewport 0 0 w h
	glClearColor 1 1 1 1
	--gameloop win cont
	--es2term cont
	
	GLFW.destroyWindow win
	GLFW.terminate

resizeCallback _ w h = return ()


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

data DrawableObject = Mesh Mesh deriving Show

awesomeObject = Mesh
	[ DrawCall
		TriangleStrip
		(Program
			[ Shader VertexShader (Blob vertexShader2)
				["pos", "color"] ["scaleRot", "offset"]
			, Shader FragmentShader (Blob fragmentShader2)
				[] []
			])
		4
		[ VertexAttr "pos" F2 FloatT False (4*2+1*4) 0 packedBlob
		, VertexAttr "color" F4 UByteT True (4*2+1*4) (4*2) packedBlob
		]
		[ UniformVar "scaleRot" (UniformMatrix2 0 0 0 0)
		, UniformVar "offset" (Uniform2f 0 0)
		]
		[]
		(DrawConfig True True 0 False)
	]

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
