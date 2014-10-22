{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}
module Main where
import Control.Applicative
import Control.Monad
import Graphics.OpenGLES
import qualified Data.ByteString.Char8 as B
import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent

-- ghc examples/billboard2.hs -lEGL -lGLESv2 -threaded && examples/billboard2
main = do
	GLFW.init
	Just win <- GLFW.createWindow 534 600 "The Billboard" Nothing Nothing
	GLFW.setFramebufferSizeCallback win $ Just (const framesize)
	forkGL
		(GLFW.makeContextCurrent (Just win) >> return False)
		(GLFW.makeContextCurrent Nothing)
		(GLFW.swapBuffers win)
	forkIO $ mapM_ (putStrLn.("# "++)) =<< glLogContents
	future <- withGL $ mkBillboard >>= mkSomeObj
	let loop c = do
		withGL $ runAction $ draw <$> future
		endFrameGL
		putStrLn . show $ c
		GLFW.pollEvents
		closing <- GLFW.windowShouldClose win
		when (not closing) $ loop (c+1)
	loop 0

data Billboard = Billboard
	{ billboard :: Program Billboard
	, mvpMatrix :: Uniform Billboard Mat4
	, tex :: Uniform Billboard Int32
	, pos :: Attrib Billboard Vec2
	, uv :: Attrib Billboard Vec2
	} deriving Typeable

mkBillboard :: GL Billboard
mkBillboard = do
	Finished p <- glCompile NoFeedback
		[ vertexShader "bb.vs" vsSrc
		, fragmentShader "bb.fs" fsSrc ]
		$ \prog step msg bin ->
			putStrLn $ "> step " ++ show step ++ ", " ++ msg
	Billboard p <$> uniform "mvpMatrix" <*> uniform "tex_unit"
		<*> attrib "pos" <*> attrib "uv"

vsSrc = B.pack $
	"#version 100\n" ++
	"uniform mat4 mvpMatrix;\n" ++
	"attribute vec2 pos;\n" ++
	"attribute vec2 uv;\n" ++
	"varying vec4 color;\n" ++
	"varying vec2 tex_coord;\n" ++
	"void main() {\n" ++
	"    gl_Position = mvpMatrix*vec4(pos, 0.0, 1.0);\n" ++
	"    color = vec4(1.0-uv.x, 1.0-uv.y, 1.0, 1.0);\n" ++
	"    tex_coord = uv;\n" ++
	"}\n"

fsSrc = B.pack $
	"#version 100\n" ++
	"precision mediump float;\n" ++
	"uniform sampler2D tex_unit;\n" ++
	"varying vec4 color;\n" ++
	"varying vec2 tex_coord;\n" ++
	"void main() {\n" ++
	"    gl_FragColor = color * texture2D(tex_unit, tex_coord);\n" ++
	"}\n"

data SomeObj = SomeObj
	{ prog :: Billboard
	, vao :: VertexArray Billboard
	, posBuf :: Buffer Vec2
	, uvBuf :: Buffer (V2 Word8)
	, ixBuf :: Buffer Word8
	, tex0 :: Texture ()
	}

mkSomeObj :: Billboard -> GL SomeObj
mkSomeObj prog@Billboard{..} = do
	posBuf <- glLoad app2gl posData
	uvBuf <- glLoad app2gl uvData
	vao <- glVA [ pos &= posBuf, uv &= uvBuf]
	tex0 <- glLoadKtxFile "white_fox.ktx" -- replace with your favorite one
	setSampler tex0 (Sampler (tiledRepeat,tiledRepeat,Nothing) 16.0 (magLinear,minLinear))
	ixBuf <- glLoad app2gl [0,1,2, 3,2,1]
	return SomeObj {..}

posData = [V2 (-1) 1, V2 1 1, V2 (-1) (-1), V2 1 (-1)]
uvData = [V2 0 0, V2 1 0, V2 0 1, V2 1 1]

draw :: SomeObj -> GL ()
draw SomeObj{..} = do
	clear [] colorBuffer
	let Billboard{..} = prog
	r <- glDraw drawTriangles billboard
		[texSlot 0 tex0] --[ begin culling, cullFace hideBack]
		[ mvpMatrix $= eye4, tex $= 0 ]
		vao $ byIndex ixBuf 0 6
	putStrLn . show $ r