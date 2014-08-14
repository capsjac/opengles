{-# LANGUAGE CPP, GADTs, Rank2Types #-}
-- | Read the OpenGL ES man pages for detail.
-- <http://www.khronos.org/opengles/sdk/docs/man31/>
-- <http://www.khronos.org/files/opengles3-quick-reference-card.pdf>
module Graphics.OpenGLES.Refactored where
-- export Data.Int, Data.Word
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
import Graphics.OpenGLES.Env

-- Performance Memo
-- WRONG -> L1Cache is fast. RIGHT -> Main memory is slow.
-- 
-- data Foo = Foo Char Int deriving (Data,Typeable,Show)
-- will add 20KB per derivation. -O make things worse: 36KB
-- Conclusion: Avoid using meta functions in busy loop

-- * Core Data Types

type GLName = CString -- eliminate hungry [Char]s. +rewrite rule.
type GLError = GLenum

type BufferObj = ForeignPtr GLuint
type ProgramObj = ForeignPtr GLuint
type ShaderObj = ForeignPtr GLuint
type TextureObj = ForeignPtr GLuint

data GLCall a where
	DrawCall
		:: !DrawMode
		-> !ProgramObj
		-> ![GraphicState]
		-> ![forall a. (UniformVar a, UniformValue a)]
		-> ![(VertexAttr, VertexBuffer b)]
		-> !VertexPicker
		-> GLCall GLError
	DrawTextureExt -- XXX test: may not work with 2.0+ context.
		:: !TextureObj
		-> !Int32
		-> !Int32
		-> !Int32
		-> !Int32
		-> !Int32
		-> !Int32
		-> !Int32
		-> !Int32
		-> !Int32
		-> GLCall GLError
	-- XXX integrate as buffer operations?
	ViewPort -- ^ Cliping rendering area. The origin is left-bottom.
		:: !Int32 -- ^ x
		-> !Int32 -- ^ y
		-> !Int32 -- ^ w
		-> !Int32 -- ^ h
		-> GLCall GLError
	SetGraphicState :: ![GraphicState] -> GLCall GLError
	FlushCommandQ :: GLCall () -- ^ Use with care. It can be expensive.
	FinishCommands :: GLCall () -- ^ Use with care. It can be expensive.
	-- .|WaitForFinishTimeout Bool Int64
	-- .^ ES 3.0 required. set GL_SYNC_FLUSH_COMMANDS_BIT or not, timeout in nanoseconds
	-- .| EnsureGPUFinish -- ^ ES 3.0 required. Nonblock
	-- FenceGPUApple(blockgpuwhiledraw)
	--waitForGPUCommandsComplete(GL_SYNC_GPU_COMMANDS_COMPLETE)
	Clear
		:: !ClearBufferFlags
		-> GLCall GLError
	GetUniform :: ProgramObj -> GLName -> GLCall UniformVar
	GetAttrib :: ProgramObj -> GLName -> GLCall VertexAttr
	GenShader
		:: Shader
		-> GLCall (Either String ShaderObj)
	GenProgram
		:: GLName
		-> [ShaderObj]
		-> Maybe (TransformFeedbackComponent, [GLName])
		-> GLCall (Either String ProgramObj)
	GenBuffer :: BufferType -> BufferUsage -> GLCall (BufferObj a)
	Upload :: BufferObj a -> a -> GLCall GLError
	GenTexture :: [B.ByteString] -> GLCall (Either String TextureObj)
	-- GenFramebuffer :: GLCall FBO
	-- ModFramebuffer :: GLCall GLError
	-- GenRenderbuffer :: RenderbufferUsage -> GLCall RBO
	-- SetFramebuffer in GraphicState?
	-- BeginQuery :: QueryType -> GLCall GLError
	-- EndQuery :: GLCall ()
	GenTransformFeedback :: [Buffer] -> GLCall TransformFeedback
	BeginTransformFeedback :: TransformFeedback -> DrawMode -> GLCall ()
	EndTransformFeedback :: GLCall ()
	deriving (Read, Show)


-- ** Draw Modes

newtype DrawMode = DrawMode Int32 deriving (Read, Show)
asPoints = DrawMode 0
asLines = DrawMode 1
asLineLoop = DrawMode 2
asLineStrip = DrawMode 3
asTriangles = DrawMode 4
asTriangleStrip = DrawMode 5
asTriangleFan = DrawMode 6

-- ** Shader
-- | [String] A program name. This name will be shown in error messages and used as binary cache key.
-- 
-- [Shaders] One or more 'VertexShader' and 'FragmentShader' can be specified.
-- Note: Only 1 main() is allowed for each type of shaders.
--data Program' = (ref, name)
--	deriving (Read, Show)
	-- GLManager type: setCompiledProgs[(name,blob)], getAllProgramBinaries

-- | [String] Name of the shader.
-- [ByteString] Shader's source code. Encoding is UTF-8 (ES 3.0+) or ASCII (2.0).
-- On using TransformFeedbacks, ...
data Shader =
	  VertexShader String B.ByteString
	-- | VertexShaderTF String B.ByteString [String]
	| FragmentShader String B.ByteString
	| ComputeShader String B.ByteString
	-- TessellationShader String B.ByteString -- ES3.1+ & GL_EXT_tessellation_shader
	deriving (Read, Show)

-- ** Graphic States

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

newtype CullFace = CullFaceSide Int32 deriving (Read, Show)
frontFace = CullFaceSide 0x0404
backFace = CullFaceSide 0x0405
frontAndBack = CullFaceSide 0x408

newtype CompFunc = CompFunc deriving (Read, Show)
glNever    = CompFunc 0x0200
glLess     = CompFunc 0x0201
glEqual    = CompFunc 0x0202
glLessEq   = CompFunc 0x0203
glGreater  = CompFunc 0x0204
glNotEq    = CompFunc 0x0205
glGreatEq  = CompFunc 0x0206
glAlways   = CompFunc 0x0207

newtype StencilOp = StencilOp_ Int32 deriving (Read, Show)
opZero     = StencilOp_ 0x0000
opKeep     = StencilOp_ 0x1E00
opReplace  = StencilOp_ 0x1E01
opIncr      = StencilOp_ 0x1E02
opDecr     = StencilOp_ 0x1E03
opInvert   = StencilOp_ 0x150A
opIncrWrap = StencilOp_ 0x8507
opDecrWrap = StencilOp_ 0x8508

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

-- ** Sampler

-- 2d vs. 3d / mag + min vs. max
-- | (Texture wrap mode, A number of ANISOTROPY filter sampling points
-- (specify 0 to disable anisotropy filter), (Fallback) Mag and Min filters).
-- 
-- When /EXT_texture_filter_anisotropic/ is not supported, fallback filters
-- are used instead.
data Sampler =
	  Sampler2D (WrapMode, WrapMode) Int32 (MagFilter, MinFilter)
	| Sampler3D (WrapMode, WrapMode, WrapMode) Int32 (MagFilter, MinFilter)
	deriving (Read, Show)

newtype MagFilter = MagFilter Int32 deriving (Read, Show)
magNearest = MagFilter 0x2600
magLinear = MagFilter 0x2601

newtype MinFilter = MinFilter Int32 deriving (Read, Show)
minNearest = MinFilter 0x2600
minLinear = MinFilter 0x2601
nearestMipmapNearest = MinFilter 0x2700
linearMipmapNearest = MinFilter 0x2701
nearestMipmapLinear = MinFilter 0x2702
linearMipmapLinear = MinFilter 0x2703

-- TODO: NPOT => ClampToEdge only check / has ext?
newtype WrapMode = WrapMode Int32 deriving (Read, Show)
repeatTexture = WrapMode 0x2901
clampToEdge = WrapMode 0x812F
mirroredRepeat = WrapMode 0x8370

-- ** Vertex Attribute

type VertexAttr a = (!GLint, String, VertexType)

data VertexType =
	  Vertex GLDimention IsNormalized
	-- ^ For /vec[1234]/, one float vector per vertex. If
	-- 'IsNormalized' is True, values are clamped to [0,1] ([-1,1] if signed.)
	| IntVertex GLDimention
	-- ^ For /ivec[1234]/, Integer variant of Vertex.
	deriving (Read, Show)

newtype GLDimention = GLDimention GLint deriving (Read, Show)
gl1 = GLDimention 1
gl2 = GLDimention 2
gl3 = GLDimention 3
gl4 = GLDimention 4
-- XXX mat4? vec1[4]? allowed?

type IsNormalized = Bool

-- ** Buffer

-- | (attribute_index_in_buffer, buffer)
type VertexBuffer a = (!Int, !Buffer a)

newtype Buffer a =
	Buffer (!BufferObj, !GLint, ![(!GLenum, !GLint, !GLint, !Word32)], !GLint)
	deriving (Read, Show)
	-- (ref, count, [(typ, dim, offset, num_instances)], stride)
	-- add field_name :: String ?
	-- ^ Treat as: let vertexList' = concatMap (replicate instanceNum) vertexList
{-
	| ConstAttr1f !Float
	| ConstAttr2f !Vec2
	| ConstAttr3f !Vec3
	| ConstAttr4f !Vec4
	| ConstAttr4i !Int32 !Int32 !Int32 !Int32 -- ^ introduced in ES 3.0
	| ConstAttr4ui !Word32 !Word32 !Word32 !Word32 -- ^ introduced in ES 3.0
-- | Vector array
	  ByteV Int32 [Int8]
	| UByteV Int32 [Word8]
	| ShortV Int32 [Int16]
	| UShortV Int32 [Word16]
	| IntV Int32 [Int32]  -- ^ introduced in ES 3.0
	| UIntV Int32 [Word32]  -- ^ introduced in ES 3.0
	| HalfFloatV Int32 [Word16]
	| FloatV Int32 [Float]
	| FixedV Int32 [Int32]
	| I2_10_10_10V [Int32]
	| UI2_10_10_10V [Word32]
	| BlobSliced VertexData B.ByteString
	-}

newtype HalfFloat = HalfFloat Word16 deriving (Read, Show)
newtype FixedFloat = FixedFloat Int32 deriving (Read, Show)
newtype I2_10_10_10V = I2_10_10_10V Int32 deriving (Read, Show)
newtype UI2_10_10_10V = UI2_10_10_10V Int32 deriving (Read, Show)

-- ** Uniform Variable

type UniformVar a = (!GLint, String)

-- | bvec[1-4] <- [1-4]{f,i,ui}v?, sampler <- 1iv?
-- Matrices are /row-major/ by default against OpenGL standard.
data UniformValue a where
	  Uniform1f :: !Float -> UniformValue Float
	| Uniform2f :: !Vec2 -> UniformValue Vec2
	| Uniform3f :: !Vec3 -> UniformValue Vec3
	| Uniform4f :: !Vec4 -> UniformValue Vec4
	| Uniform1fv :: ![Float] -> UniformValue [Float]
	| Uniform2fv :: ![Vec2] -> UniformValue [Vec2]
	| Uniform3fv :: ![Vec3] -> UniformValue [Vec3]
	| Uniform4fv :: ![Vec4] -> UniformValue [Vec4]
	| Uniform1i :: !Int32 -> UniformValue Int32
	| Uniform2i :: !(Int32, Int32) -> UniformValue (Int32, Int32)
	 -- should make sth like IVec2?
	| Uniform3i :: !(Int32, Int32, Int32) -> UniformValue (Int32, Int32, Int32)
	| Uniform4i :: !(Int32, Int32, Int32, Int32) -> UniformValue (Int32, Int32, Int32, Int32)
	| Uniform1iv :: ![Int32] -> UniformValue [Int32]
	| Uniform2iv :: ![(Int32, Int32)] -> UniformValue [(Int32, Int32)]
	| Uniform3iv :: ![(Int32, Int32, Int32)] -> UniformValue [(Int32, Int32, Int32)]
	| Uniform4iv :: ![(Int32, Int32, Int32, Int32)] -> UniformValue [(Int32, Int32, Int32, Int32)]
	| UniformMat2 :: !Mat2 -> UniformValue Mat2
	| UniformMat3 :: !Mat3 -> UniformValue Mat3
	| UniformMat4 :: !Mat4 -> UniformValue Mat4
	| UniformMat2v :: ![Mat2] -> UniformValue [Mat2]
	| UniformMat3v :: ![Mat3] -> UniformValue [Mat3]
	| UniformMat4v :: ![Mat4] -> UniformValue [Mat4]
	| UniformTexture :: !Texture -> UniformValue Texture
	| UniformTextures :: ![Texture] -> UniformValue [Texture]
	-- ES 3.0
	| Uniform1ui :: !Word32 -> UniformValue Word32
	| Uniform2ui :: !(Word32, Word32) -> UniformValue (Word32, Word32)
	| Uniform3ui :: !(Word32, Word32, Word32) -> UniformValue (Word32, Word32, Word32)
	| Uniform4ui :: !(Word32, Word32, Word32, Word32) -> UniformValue (Word32, Word32, Word32, Word32)
	| Uniform1uiv :: ![Word32] -> UniformValue [Word32]
	| Uniform2uiv :: ![(Word32, Word32)] -> UniformValue [(Word32, Word32)]
	| Uniform3uiv :: ![(Word32, Word32, Word32)] -> UniformValue [(Word32, Word32, Word32)]
	| Uniform4uiv :: ![(Word32, Word32, Word32, Word32)] -> UniformValue [(Word32, Word32, Word32, Word32)]
	| UniformMat2x3 :: !(Vec2, Vec2, Vec2)  -> UniformValue (Vec2, Vec2, Vec2) -- not sure these are good
	| UniformMat3x2 :: !(Vec3, Vec3)  -> UniformValue (Vec3, Vec3) -- [Float]
	| UniformMat2x4 :: !(Vec2, Vec2, Vec2, Vec2) -> UniformValue (Vec2, Vec2, Vec2, Vec2)
	| UniformMat4x2 :: !(Vec4, Vec4) -> UniformValue (Vec4, Vec4)
	| UniformMat3x4 :: !(Vec3, Vec3, Vec3, Vec3) -> UniformValue (Vec3, Vec3, Vec3, Vec3)
	| UniformMat4x3 :: !(Vec4, Vec4, Vec4) -> UniformValue (Vec4, Vec4, Vec4)
	| UniformMat2x3v :: ![(Vec2, Vec2, Vec2)] -> UniformValue [(Vec2, Vec2, Vec2)]
	| UniformMat3x2v :: ![(Vec3, Vec3)] -> UniformValue [(Vec3, Vec3)]
	| UniformMat2x4v :: ![(Vec2, Vec2, Vec2, Vec2)] -> UniformValue [(Vec2, Vec2, Vec2, Vec2)]
	| UniformMat4x2v :: ![(Vec4, Vec4)] -> UniformValue [(Vec4, Vec4)]
	| UniformMat3x4v :: ![(Vec3, Vec3, Vec3, Vec3)] -> UniformValue [(Vec3, Vec3, Vec3, Vec3)]
	| UniformMat4x3v :: ![(Vec4, Vec4, Vec4)] -> UniformValue [(Vec4, Vec4, Vec4)]
	deriving (Read, Show)

-- ** Vertex Picker

-- | VFromCounts -> VFromCounts', VIndex8/16/32 -> VIndex(Instanced)',
-- VIndices8/16/32 -> VIndices' or DrawCallSequence[VIndex(Instanced)']
--
-- Version/Ext fallback feature is not yet. (See above)
data VertexPicker =
	  TakeFrom
		!Int32 -- ^ Start Index
		!Word32 -- ^ A number of vertices to
	| TakeFromInstanced !Int32 !Word32 !Word32
	| TakeFromMany ![(Int32, Word32)] -- XXX native array
	| TakeFromMany' !B.ByteString !B.ByteString
	-- ^ Wrapping glMultiDrawArraysEXT
	| ByIndex8 ![Word8]
	| ByIndex16 ![Word16]
	| ByIndex32 ![Word32]
	| ByIndex' !BufferObj !GLsizei !GLenum !Int
	-- ^ Wrapping glDrawElements
	| ByIndexInstanced8 ![Word8] !Int
	| ByIndexInstanced16 ![Word16] !Int
	| ByIndexInstanced32 ![Word32] !Int
	| ByIndexInstanced' !BufferObj !GLsizei !GLenum !Int !GLsizei
	-- ^ Wrapping glDrawElementsInstanced
	| ByIndices8 ![[Word8]]
	| ByIndices16 ![[Word16]]
	| ByIndices32 ![[Word32]]
	| ByIndices' !B.ByteString !GLenum !B.ByteString
	-- ^ Wrapping glMultiDrawElementsEXT
	-- .| VFromToIndex8/16/32 !Int !Int ![Word8/16/32]
	-- .| VFromToIndex' !BufferRef !Int !Int !GLsizei !GLenum !Int
	-- .^ Wrapping glDrawRangeElements
	| DrawCallSequence [VertexPicker] -- allow lazy evalution
	deriving (Read, Show)

-- ** ClearBuffer

newtype ClearBufferFlags = ClearBufferFlag Int32 deriving (Num, Read, Show)
clearDepth = ClearBufferFlag 0x100
clearStencil = ClearBufferFlag 0x400
clearColor = ClearBufferFlag 0x4000


-- * Data-driven Rendering
-- ** Initialization

data GLManager = GLManager
	{ glAPIVersion :: Int -- ^ set API version to be used
	, glLogger :: (GLCall a, GLFeedback a) -> IO ()
	, glProgramCache :: IORef [(String, ProgramBinary)]
	-- ^ get/set after/before program linkage
	} deriving Show

type ProgramBinary = B.ByteString
instance Show (IORef [(String, ProgramBinary)]]) where
	show = const "(IORef [(String, ProgramBinary)])"

initGLManager :: IO GLManager
initGLManager = newIORef [] >>= return . GLManager ver (return ()) 
	where ver = case (glEnv majorVersion, glEnv minorVersion) of
		(0, 0) -> 20
		(major, minor) -> major * 10 + minor

setProgramCache :: [(String, ProgramBinary)] -> GLManager -> IO ()
setProgramCache cache glm = writeIORef cache (glProgramCache glm)

-- ** Run GLCalls
data GLFeedback
postGLCall :: GLCall a -> IO (IORef (Maybe (GLFeedback a))

withGLCall :: GLCall a -> @GLThread@ (GLFeedback a -> IO ()) -> IO ()

runGLCall' :: GLCall a -> IO ()

runGLCall :: GLCall a -> IO (GLFeedback a)

get :: GLFeedback a -> IO a
get () = .

altRes :: GLFeedback a -> a -> IO a


type GLIO a = EitherT [String] IO a

loadProgram :: GLManager
             -> GLName
             -> [Shader]
             -> Maybe (TransformFeedbackComponent, [GLName])
             -> ([Shader] -> Int -> Maybe ProgramBinary -> IO ())
             -> GLIO (ProgramObj, [ShaderObj])

-- ** Draw
drawData :: GLCall a -> IO ()
drawData (DrawCall (DrawMode mode) prog conf uniforms attribs picker) = do
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

drawData (DrawTextureExt ref u v tw th x y z w h) = do
	-- GL_TEXTURE_2D = 0x0DE1
	withForeignPtr ref (glBindTexture 0x0DE1 . ptrToId)
	-- GL_TEXTURE_CROP_RECT_OES = 0x8B9D
	withArray [u, v, tw, th] (glTexParameteriv 0x0DE1 0x8B9D)
	glDrawTexiOES x y z w h -- disable AlphaTest?

-- ** Get parameter

-- * Internals
-- ** Garbage collection on GL Objects

-- | Objects are sharable among threads using shared_contex.
type GLObject a = (ForeignPtr Word32, String)
data GLFeedback a where
	GLError' :: GLError -> GLFeedback GLError
	TransformFeedback' :: TransformFeedback -> GLFeedback TransformFeedback
	GetUnifrom' :: GLint -> GLFeedback GLint
	GetAttrib' :: GLint -> GLFeedback GLint
	deriving (Read, Show)
-- forceFreeGLObjectsNow :: forall a. [GLObject a] -> IO ()
data TransformFeedback = TransformFeedback deriving (Read, Show) -- XXX
bindFinalizer :: IO () -> GLuint -> IO (ForeignPtr GLuint)
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

setUniformVar :: (UniformVar, UniformValue) -> IO ()
setUniformVar ((loc, _), val) = case val of
	Uniform1f x -> glUniform1f loc x
	Uniform2f (Vec2 x y) -> glUniform2f loc x y
	Uniform3f (Vec3 x y z) -> glUniform3f loc x y z
	Uniform4f (Vec4 x y z w) -> glUniform4f loc x y z w
	Uniform1fv xs -> withArray xs (glUniform1fv loc (len xs) . castPtr)
	Uniform2fv xs -> withArray xs (glUniform2fv loc (len xs) . castPtr)
	Uniform3fv xs -> withArray xs (glUniform3fv loc (len xs) . castPtr)
	Uniform4fv xs -> withArray xs (glUniform4fv loc (len xs) . castPtr)
	Uniform1i x -> glUniform1i loc x
	Uniform2i (x,y) -> glUniform2i loc x y
	Uniform3i (x,y,z) -> glUniform3i loc x y z
	Uniform4i (x,y,z,w) -> glUniform4i loc x y z w
	{-Uniform1iv xs -> withArray xs (glUniform1iv loc (len xs))
	Uniform2iv xs -> withArray xs (glUniform2iv loc (len xs))
	Uniform3iv xs -> withArray xs (glUniform3iv loc (len xs))
	Uniform4iv xs -> withArray xs (glUniform4iv loc (len xs))-}
	UniformMat2 m -> with m (glUniformMatrix2fv loc 1 1 . castPtr)
	UniformMat3 m -> with m (glUniformMatrix3fv loc 1 1 . castPtr)
	UniformMat4 m -> with m (glUniformMatrix4fv loc 1 1 . castPtr)
	UniformMat2v mx -> withArray mx (glUniformMatrix2fv loc (len mx) 1 . castPtr)
	UniformMat3v mx -> withArray mx (glUniformMatrix3fv loc (len mx) 1 . castPtr)
	UniformMat4v mx -> withArray mx (glUniformMatrix4fv loc (len mx) 1 . castPtr)
	--UniformTexture (Texture' _ tex) -> do
		--glActiveTexture(0-31)
		--glBindTexture 2D texRef
		--glBindSampler ...
		-- glUniform1i unifid 0-31
		--glUniform1i loc x
	--UniformTextures texes -> withArray xs (glUniform1iv loc (len xs))
	Uniform1ui x -> glUniform1ui loc x
	Uniform2ui (x,y) -> glUniform2ui loc x y
	Uniform3ui (x,y,z) -> glUniform3ui loc x y z
	Uniform4ui (x,y,z,w) -> glUniform4ui loc x y z w
	{-Uniform1uiv xs -> withArray xs (glUniform1uiv loc (len xs))
	Uniform2uiv xs -> withArray xs (glUniform2uiv loc (len xs))
	Uniform3uiv xs -> withArray xs (glUniform3uiv loc (len xs))
	Uniform4uiv xs -> withArray xs (glUniform4uiv loc (len xs))-}
	{-- ES 3.0
	| UniformMat2x3 !(Vec2, Vec2, Vec2)
	| UniformMat3x2 !(Vec3, Vec3) -- [Float]
	| UniformMat2x4 !(Vec2, Vec2, Vec2, Vec2)
	| UniformMat4x2 !(Vec4, Vec4)
	| UniformMat3x4 !(Vec3, Vec3, Vec3, Vec3)
	| UniformMat4x3 !(Vec4, Vec4, Vec4)
	| UniformMat2x3v ![(Vec2, Vec2, Vec2)]
	| UniformMat3x2v ![(Vec3, Vec3)]
	| UniformMat2x4v ![(Vec2, Vec2, Vec2, Vec2)]
	| UniformMat4x2v ![(Vec4, Vec4)]
	| UniformMat3x4v ![(Vec3, Vec3, Vec3, Vec3)]
	| UniformMat4x3v ![(Vec4, Vec4, Vec4)]
	-}
	where
		len = fromIntegral . length

setVertexAttr :: (VertexAttr, VertexBuffer a) -> IO ()
setVertexAttr (loc, va) = case va of
	Vertex _ va -> error "setVertexAttr: VertexAttr must be compiled!"
	IntVertex _ va -> error "setVertexAttr: VertexAttr must be compiled!"
{-}	NormalizedVertex _ va -> error "setVertexAttr: VertexAttr must be compiled!"
	Instanced divNum va -> do
		setVertexAttr (loc, va)
		glVertexAttribDivisor divNum loc
	BufferSlice _ ref size typ normalize stride offset -> do
		withForeignPtr ref (glBindBuffer 0x8892 . ptrToId)
		glVertexAttribPointer loc size typ normalize stride (idToPtr offset)
		showError "glVertexAttribPointer"
		glEnableVertexAttribArray loc
		showError "glEnableVertexAttribArray"
	BufferSlicei _ ref size typ stride offset -> do
		withForeignPtr ref (glBindBuffer 0x8892 . ptrToId)
		glVertexAttribIPointer loc size typ stride (idToPtr offset)
		glEnableVertexAttribArray loc
	ConstAttr1f _ x -> d >> glVertexAttrib1f loc x
	ConstAttr2f _ (Vec2 x y) -> d >> glVertexAttrib2f loc x y
	ConstAttr3f _ (Vec3 x y z) -> d >> glVertexAttrib3f loc x y z
	ConstAttr4f _ (Vec4 x y z w) -> d >> glVertexAttrib4f loc x y z w
	ConstAttr4i _ x y z w -> d >> glVertexAttribI4i loc x y z w
	ConstAttr4ui _ x y z w -> d >> glVertexAttribI4ui loc x y z w
	where
		d = glDisableVertexAttribArray loc-}

invokeDraw :: DrawMode -> VertexPicker -> IO ()
invokeDraw (DrawMode mode) picker = case picker of
	TakeFrom first count -> do
		glDrawArrays mode first count
	TakeFromInstanced first count primcount ->
		glDrawArraysInstanced mode first count primcount
	TakeFromMany list ->
		forM_ list (\(fst, cnt) -> glDrawArrays mode fst cnt)
	--TakeFromMany' firstbs countbs ->
	--	withBSBS countbs firstbs $ \cptr fptr clen ->
	--		glMultiDrawArraysEXT mode fptr cptr (unsafeShiftR clen 2)
	ByIndex' ref count typ offset -> do
		-- bind GL_ELEMENT_ARRAY_BUFFER = 0x8893
		withForeignPtr ref (glBindBuffer 0x8893 . ptrToId)
		glDrawElements mode count typ (idToPtr offset)
	ByIndexInstanced' ref count typ offset divNum ->
		glDrawElementsInstanced mode count typ (idToPtr offset) divNum
	--VIndices8/16/32 ... ->
	--ByIndices' countbs typ indicesbs ->
	--	withBSBS countbs indicesbs $ \cptr iptr clen ->
	--		glMultiDrawElementsEXT mode cptr typ iptr (unsafeShiftR clen 2)
	-- VFromToIndex' ...
	DrawCallSequence xs -> mapM_ (invokeDraw mode) xs
	_ -> error $ "invokeDraw: VertexPicker (" ++ show picker ++ ") must be compiled."



newtype TransformFeedbackComponent = TransformFeedbackComponent GLenum
	deriving (Read, Show)
interleavedAttribs = TransformFeedbackComponent 0x8C8C
separateAttribs = TransformFeedbackComponent 0x8C8D

newtype BufferUsage = BufferUsage GLenum deriving (Read, Show)
staticDraw = BufferUsage 0x88E4
dynamicDraw = BufferUsage 0x88E8
streamDraw = BufferUsage 0x88E0
