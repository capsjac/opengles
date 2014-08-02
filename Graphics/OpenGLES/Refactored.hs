{-# LANGUAGE CPP, GADT #-}
module Graphics.OpenGLES.Refactored where
import Control.Applicative
import Control.Monad
import qualified Data.ByteString as B
import Data.ByteString.Internal (create, nullForeignPtr, toForeignPtr)
import Data.Either
import Data.IORef
import Data.Marshal
import Data.Vect
import Foreign hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.C.String
import Foreign.Concurrent -- (GHC only)
import Graphics.OpenGLES.Base


-- * Core Data Types

data GLCall =
	  DrawCall
		!DrawMode !Program ![GraphicState]
		![UniformVar] ![(AttrId, VertexAttr)] !VertexPicker
	| DrawTexture !TextureRef !Int32 !Int32 !Int32 !Int32 !Int32 !Int32 !Int32 !Int32 !Int32
	| SetGraphicState ![GraphicState]
	-- .|WaitForFinishTimeout Bool Int64
	-- .^ ES 3.0 required. set GL_SYNC_FLUSH_COMMANDS_BIT or not, timeout in nanoseconds
	-- .| EnsureGPUFinish -- ^ ES 3.0 required. Nonblock
	-- FenceGPUApple
	| ClearBuffer !ClearBufferBits
	-- GenShader
	-- GenProgram
	-- GenBuffer
	-- GenTexture
	-- BeginQuery
	-- EndQuery
	-- BeginTransformFeedback
	-- EndTransformFeedback
	deriving (Read, Show)


type GLIO a = EitherT [String] IO a

loadProgram :: GLManager -> String -> [Shader] -> ([Shader] -> Int -> IO ())
             -> GLIO (ProgramRef, [ShaderRef])

-- ** Graphic State

-- | Draw configurations on Rasterization and Per-Fragment Operations.
-- Note: Graphic state is sticky.
data GraphicState =
	  Begin Capability -- ^ Enable capability persistently.
	| End Capability -- ^ Disable capability persistently.

	| LineWidth Float
	| FrontFace Bool
	-- ^ Whether counter-wise or not
	| CullFace CullFace
	-- ^ Which side of polygons should be rasterized, cannot used with DepthTest
	| PolygonOffset Float Float -- ^ Factor and Units

	| Scissor Int32 Int32 Word32 Word32 -- ^ (!!) left, bottom, width, height
	| SampleCvrg Float Bool -- ^ float[0,1] value, invert
	| StencilFunc CompFunc Int32 Word32 -- ^ comp, mask value
	| StencilFuncSeparate CullFace CompFunc Int32 Word32
	| StencilOp StencilOp StencilOp StencilOp -- ^ sfail, dpfail, dppass
	| StencilOpSeparate CullFace StencilOp StencilOp StencilOp
	| DepthFunc CompFunc
	| BlendEquation BlendOp -- ^ mode
	| BlendEquationSeparate BlendOp BlendOp -- ^ modeRGB, modeAlpha
	| BlendFunc BlendingFactor BlendingFactor -- ^ src, dest
	| BlendFuncSeparate BlendingFactor BlendingFactor
	                    BlendingFactor BlendingFactor
	-- ^ srcRGB, dstRGB, srcAlpha, dstAlpha
	| BlendColor Float Float Float Float
	-- ^ red, green, blue and alpha value range [0,1]
	| GenerateMipmapHint Hint
	| FragmentShaderDerivativeHint Hint -- ^ introduced in ES 3.0
	deriving (Read, Show)

-- ** Drawing

newtype DrawMode = DrawMode Int32 deriving (Read, Show)
asPoints = DrawMode 0
asLines = DrawMode 1
asLineLoop = DrawMode 2
asLineStrip = DrawMode 3
asTriangles = DrawMode 4
asTriangleStrip = DrawMode 5
asTriangleFan = DrawMode 6

newtype Capability = Capability Int32 deriving (Read, Show)
cullFaceTest = Capability 0x0B44
blend = Capability 0x0BE2
dither = Capability 0x0BD0
stencilTest = Capability 0x0B90
depthTest = Capability 0x0B71
scissorTest = Capability 0x0C11
polygonOffsetFill = Capability 0x8037
sampleAlphaToCoverage = Capability 0x809E
sampleCoverage = Capability 0x80A0
primitiveRestartFixedIndex = Capability 0x8D69
rasterizerDiscard = Capability 0x8C89

newtype CullFaceSide = CullFace Int32 deriving (Read, Show)
frontFace = CullFace 0x0404
backFace = CullFace 0x0405
frontAndBack = CullFace 0x408

newtype CompFunc = CompFunc deriving (Read, Show)
glNever    = CompFunc 0x0200
glLess     = CompFunc 0x0201
glEqual    = CompFunc 0x0202
glLessEq   = CompFunc 0x0203
glGreater  = CompFunc 0x0204
glNotEq    = CompFunc 0x0205
glGreatEq  = CompFunc 0x0206
glAlways   = CompFunc 0x0207

newtype StencilOp = StencilOp Int32 deriving (Read, Show)
opZero     = StencilOp 0x0000
opKeep     = StencilOp 0x1E00
opReplace  = StencilOp 0x1E01
opIncr      = StencilOp 0x1E02
opDecr     = StencilOp 0x1E03
opInvert   = StencilOp 0x150A
opIncrWrap = StencilOp 0x8507
opDecrWrap = StencilOp 0x8508

newtype BlendOp = BlendOp Int32 deriving (Read, Show)
addBlending        = BlendOp 0x8006
subBlending        = BlendOp 0x800A
reverseSubBlending = BlendOp 0x800B

newtype BlendingFactor = BlendingFactor Int32 deriving (Read, Show)
factorZero         = BlendingFactor 0
factorOne          = BlendingFactor 1
srcColor           = BlendingFactor 0x300
oneMinusSrcColor   = BlendingFactor 0x301
srcAlpha           = BlendingFactor 0x302
oneMinusSrcAlpha   = BlendingFactor 0x303
dstAlpha           = BlendingFactor 0x304
oneMinusDstAlpha   = BlendingFactor 0x305
dstColor           = BlendingFactor 0x306
oneMinusDstColor   = BlendingFactor 0x307
srcAlphaSaturate   = BlendingFactor 0x308
constColor         = BlendingFactor 0x8001
oneMinusConstColor = BlendingFactor 0x8002
constAlpha         = BlendingFactor 0x8003
oneMinusConstAlpha = BlendingFactor 0x8004

newtype Hint = Hint Int32 deriving (Read, Show)
dontCare = Hint 0x1100
fastest  = Hint 0x1101
nicest   = Hint 0x1102

newtype ClearBufferFlags = ClearBufferFlag Int32 deriving (BIts, Read, Show)
clearDepth = ClearBufferFlag 0x100
clearStencil = ClearBufferFlag 0x400
clearColor = ClearBufferFlag 0x4000


-- * Data-driven Rendering
-- ** Initialization

data GLManager = GLManager
	{ glAPIVersion :: GLVersion -- ^ set API version to be used
	, compiledProgramCache :: IORef [(String, B.ByteString)]
	-- ^ get/set after/before program linkage
	--, glRefHolder :: [ForeignPtr GLuint]
	} deriving Show
instance Show (IORef a) where show = const "(IORef ..)"

data GLVersion = ES2 | ES3 | ES3_1 deriving Show

initGLManager :: IO GLManager
initGLManager = newIORef [] >>= return . GLManager ES2 

-- ** Draw
drawData :: DrawCall -> IO ()
drawData (DrawCall mode prog conf uniforms attribs picker) = do
	-- draw config
	mapM_ setGraphicState conf

	-- shader setting
	--let Program _ _ progRef _ = prog
	--withForeignPtr progRef (glUseProgram . ptrToId)
	
	-- vertex attribute
	mapM_ setVertexAttr attribs
	
	-- uniform variable
	mapM_ setUniformVar uniforms
	
	invokeDraw mode picker

drawData (DrawTexture ref u v tw th x y z w h) = do
	-- GL_TEXTURE_2D = 0x0DE1, GL_TEXTURE_CROP_RECT_OES = 0x8B9D
	withForeignPtr ref (glBindTexture 0x0DE1 . ptrToId)
	withArray [u, v, tw, th] (glTexParameteriv 0x0DE1 0x8B9D)
	glDrawTexiOES x y z w h -- disable AlphaTest?


-- * Internals
-- ** Garbage collection on GL Objects

bindFinalizer :: IO () -> ResourceId -> IO (ForeignPtr ResourceId)
bindFinalizer f i = newForeignPtr (idToPtr i) (putStrLn "Fin" >> f)

idToPtr i = nullPtr `plusPtr` fromIntegral i
ptrToId ptr = fromIntegral $ ptr `minusPtr` nullPtr
finalize = finalizeForeignPtr

-- ** Error

getError :: IO (Maybe String)
getError = unMarshal <$> glGetError
	where unMarshal x = case x of
		0x0000 -> Nothing -- NoError
		0x0500 -> Just "InvalidEnum"
		0x0501 -> Just "InvalidValue"
		0x0502 -> Just "InvalidOperation"
		0x0505 -> Just "OutOfMemory"
		0x0506 -> Just "InvalidFrameBufferOperation"
		_ -> Just ("glGetError: Unknown error " ++ show x)

showError :: String -> IO ()
showError location =
	getError >>= (>>= \x -> putStrLn ("GLError " ++ location ++ ": " ++ x))

-- ** State Setting

setGraphicState :: GraphicState -> IO ()
setGraphicState x = case x of
	Begin cap ->
		glEnable cap
	End cap ->
		glDisable cap
	LineWidth width ->
		glLineWidth width
	FrontFace cw ->
		glFrontFace (if cw then 0x900 else 0x901)
	CullFace cf ->
		glCullFace cf
	PolygonOffset factor units ->
		glPolygonOffset factor units
	Scissor left bottom width height ->
		glScissor left bottom width height
	SampleCvrg value invert ->
		glSampleCoverage value (if invert then 1 else 0)
	StencilFunc func comp mask ->
		glStencilFunc func comp mask
	StencilFuncSeparate cull func comp mask ->
		glStencilFuncSeparate cull func comp mask
	StencilOp sfail dpfail dppass ->
		glStencilOp sfail dpfail dppass
	StencilOpSeparate cull sfail dpfail dppass ->
		glStencilOpSeparate cull sfail dpfail dppass
	DepthFunc comp ->
		glDepthFunc comp
	BlendEquation mode ->
		glBlendEquation mode
	BlendEquationSeparate rgb a ->
		glBlendEquationSeparate rgb a
	BlendFunc src dest ->
		glBlendFunc src dest
	BlendFuncSeparate srcRgb dstRgb srcAlpha dstAlpha ->
		glBlendFuncSeparate srcRgb dstRgb srcAlpha dstAlpha
	BlendColor r g b a ->
		glBlendColor r g b a
	GenerateMipmapHint hint ->
		glHint 0x8192 hint
	FragmentShaderDerivativeHint hint ->
		glHint 0x8B8B hint

invokeDraw :: DrawMode -> VertexPicker -> IO ()
invokeDraw mode picker = case picker of
	VFromCount first count -> do
		glDrawArrays mode first count
	VFromCountInstanced first count primcount ->
		glDrawArraysInstanced mode first count primcount
	VFromCounts list ->
		forM_ list (\(fst, cnt) -> glDrawArrays mode fst cnt)
	VFromCounts' firstbs countbs ->
		withBSBS countbs firstbs $ \cptr fptr clen ->
			glMultiDrawArraysEXT mode fptr cptr (unsafeShiftR clen 2)
	VIndex' ref count typ offset -> do
		-- bind GL_ELEMENT_ARRAY_BUFFER = 0x8893
		withForeignPtr ref (glBindBuffer 0x8893 . ptrToId)
		glDrawElements mode count typ (idToPtr offset)
	VIndexInstanced' ref count typ offset divNum ->
		glDrawElementsInstanced mode count typ (idToPtr offset) divNum
	--VIndices8/16/32 ... ->
	VIndices' countbs typ indicesbs ->
		withBSBS countbs indicesbs $ \cptr iptr clen ->
			glMultiDrawElementsEXT mode cptr typ iptr (unsafeShiftR clen 2)
	-- VFromToIndex' ...
	DrawCallSequence xs -> mapM_ (invokeDraw mode) xs
	_ -> error $ "invokeDraw: VertexPicker (" ++ show picker ++ ") must be compiled."


