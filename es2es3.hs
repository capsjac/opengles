{-# LANGUAGE FlexibleInstances #-}
module Main where
import Control.Concurrent
--import Graphics.EGL
import Graphics.OpenGLES
import Graphics.OpenGLES.Types
import qualified Graphics.UI.GLFW as GLFW
import Foreign.Marshal
import Foreign.Storable
import Foreign.Ptr
import System.Random

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
	GLFW.swapInterval 1
	GLFW.setFramebufferSizeCallback win $ Just $ \_ w h -> return ()
	exts <- getGLExtensions
	putStrLn $ show exts
	putStrLn "Main."
	putStrLn =<< getGLVersion
	putStrLn $ show detectGLESVersion
	cont <- es2init
	putStrLn "ready."
	glClearColor 1 1 1 1
	gameloop win cont
	es2term cont
	
	GLFW.destroyWindow win
	GLFW.terminate

maxInstancesPerSide = 16
maxInstances = 16 * 16
maxRotSpeed = 2 * 3.14159 * 0.3


-- Square with diagonal < 2 so that it fits in a [-1..1]^2 square
-- regardless of rotation.
quad :: [Vertex]
quad = 
	[((-0.7,-0.7), (0x00, 0xFF, 0x00))
	,(( 0.7,-0.7), (0x00, 0x00, 0xFF))
	,((-0.7, 0.7), (0xFF, 0x00, 0x00))
	,(( 0.7, 0.7), (0x00, 0xFF, 0xFF))
	]

beginFrame = do
	--disable Dither
	glViewport 0 0 1366 768
	glClearColor 0.2 0.2 0.3 1.0
	clearBuffer True True False

endFrame win = do
	GLFW.swapBuffers win
	GLFW.pollEvents
	threadDelay $ floor $ 1000000 * 0.01

gameloop :: GLFW.Window -> Context -> IO ()
gameloop win opt = do
	beginFrame
	opt <- es2draw opt
	endFrame win
	shouldExit <- GLFW.windowShouldClose win
	if shouldExit
		then return ()
		else gameloop win opt

vertexShader2 =
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

fragmentShader2 =
	"#version 100\n" ++
	"precision mediump float;\n" ++
	"varying vec4 vColor;\n" ++
	"void main() {\n" ++
	"    gl_FragColor = vColor;\n" ++
	"}\n"

data Context = Context Program Int Int GLint GLint Buffer
                       (Int,Int) [GLfloat] [GLfloat]
es2init = do
	maybeProgram <- createProgram
		[("vert", vertexShader2)]
		[("frag", fragmentShader2)]
	case maybeProgram of
		Left msg -> error $ "Could not create program!\n" ++ concat msg
		Right program -> do
			posAttr <- getAttribLocation program "pos"
			colorAttr <- getAttribLocation program "color"
			scaleRotUnif <- getUniformLocation program "scaleRot"
			offsetUnif <- getUniformLocation program "offset"
			showError "glGetUniformLocation"
			
			[buf] <- genObjects 1
			bindObject ArrayBuffer buf
			withArray' quad $ \arr -> do
				bufferData ArrayBuffer (length quad * sizeOf (quad !! 0))
				           arr StaticDraw	

			let (wx, hx) = calcDim 1366 768
			let elems = wx * hx
			let angles = take elems (repeat 0)
			rand <- rollDice
			let angleVs = take elems rand

			return $ Context program posAttr colorAttr scaleRotUnif offsetUnif buf (wx, hx) angles angleVs

es2term (Context p _ _ _ _ buf _ _ _) = do
	deleteObjects [buf]
	deleteProgram p

es2draw cxt@(Context program posAttr colorAttr scaleRotUnif offsetUnif buf (wx,hx) angles angleVs) = do
	useProgram program

	bindObject ArrayBuffer buf
	let instanceSize = 4*2 + 1*3
	let colorPos = 4*2
	vertexAttribPointerArrayBufBound posAttr 4 FloatT False instanceSize 0
	vertexAttribPointerArrayBufBound colorAttr 4 UByteT True instanceSize colorPos
	enableVertexAttribArray posAttr
	enableVertexAttribArray colorAttr
	
	let r_x = 1 /fromIntegral wx
	let r_y = 1 /fromIntegral hx

	mapM_ (\i -> do
		let a = angles !! i
		uniformMatrixf2 scaleRotUnif 1 False [r_x*cos a,r_y*sin a,r_x*negate(sin a),r_y*cos a]
		uniform offsetUnif (2/fromIntegral wx*(0.5+fromIntegral(i`mod`wx))-1::GLfloat,2/fromIntegral hx*(0.5+fromIntegral(i`div`wx))-1::GLfloat)
		drawArrays TriangleStrip 0 4
		) [0..wx*hx-1]

	let angles' = zipWith (+) angles angleVs
	return $ Context program posAttr colorAttr scaleRotUnif offsetUnif buf (wx,hx) angles' angleVs

rollDice :: IO [GLfloat]
rollDice = getStdGen >>= return . randomRs (0, 2*3.14*0.01)

calcDim w h =
	let n = floor $ h / (w / 16)
	in (16, n)

type Vertex = ((GLfloat,GLfloat),(GLubyte,GLubyte,GLubyte))
instance Storable Vertex where
	sizeOf ((x,y),(r,g,b)) = sizeOf x + sizeOf y + sizeOf r + sizeOf g + sizeOf b
	alignment = sizeOf
	peek _ = error "noway"
	poke p ((x,y),(r,g,b)) = do
		putStrLn "pkjoj"
		pokeByteOff p 0 x
		pokeByteOff p 4 y
		pokeByteOff p 8 r
		pokeByteOff p 9 g
		pokeByteOff p 10 b

withArray' x f = do
	arr <- mallocBytes (length x * sizeOf (x !! 0))
	pokeArray arr x
	res <- f arr
	free arr
	return res
