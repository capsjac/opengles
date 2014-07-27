{-# LANGUAGE FlexibleInstances, BangPatterns, StandaloneDeriving, Rank2Types #-}
module Graphics.OpenGLES.Core (
	DrawCall(..),
	DrawMode(..),
	Program(..),
	Shader(..),
	GraphicState(..),
	Capability(..),
	CullFace(..),
	CompFunc(..),
	StencilOp(..),
	BlendOp(..),
	BlendingFactor(..),
	Hint(..),
	UniformVar(..),
	UniformValue(..),
	Texture(..),
	TextureTarget(..),
	TextureColorFormat(..),
	TextureBitLayout(..),
	TextureInternalFormat(..),
	TextureData(..),
	Sampler(..),
	MagFilter(..),
	MinFilter(..),
	WrapMode(..),
	VertexAttr(..),
	VertexData(..),
	VertexPicker(..),
	GLManager(..),
	GLVersion(..),
	initGLManager,
	compileCall,
	drawData
) where
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

data DrawCall =
	  DrawUnit
		DrawMode
		(ProgramRef -> [ShaderRef] -> Program)
		[GraphicState]
		[UniformId -> UniformVar]
		[VertexAttr]
		VertexPicker
	| DrawCall
		!DrawMode !Program ![GraphicState]
		![UniformVar] ![(AttrId, VertexAttr)] !VertexPicker
	| DrawTexture !TextureRef !Int !Int !Int !Int !Int !Int !Int !Int !Int -- add DrawConfig?
	| SetGraphicState [GraphicState]
	-- .|WaitForFinishTimeout Bool Int64
	-- .^ ES 3.0 required. set GL_SYNC_FLUSH_COMMANDS_BIT or not, timeout in nanoseconds
	-- .| EnsureGPUFinish -- ^ ES 3.0 required. Nonblock
	deriving (Show, Eq)

instance Show (ProgramRef -> [ShaderRef] -> Program) where show x = show (x nullFPtr [])
instance Show (AttrId -> VertexAttr) where show x = show (x 0)
instance Show (UniformId -> UniformVar) where show x = show (x 0)
instance Show (TextureRef -> Texture) where show x = show (x nullFPtr)
instance Eq (ProgramRef -> [ShaderRef] -> Program) where x == y = x nullFPtr [] == y nullFPtr []
instance Eq (AttrId -> VertexAttr) where x == y = x 0 == y 0
instance Eq (UniformId -> UniformVar) where x == y = x 0 == y 0
instance Eq (TextureRef -> Texture) where x == y = x nullFPtr == y nullFPtr

nullFPtr = castForeignPtr nullForeignPtr
type BufferRef = ForeignPtr GLuint
type ProgramRef = ForeignPtr GLuint
type ShaderRef = ForeignPtr GLuint
type AttrId = GLuint
type UniformId = GLint
type TextureRef = ForeignPtr GLuint
type ResourceId = GLuint


-- ** Draw mode

data DrawMode = Points | Lines | LineLoop | LineStrip
              | Triangles | TriangleStrip | TriangleFan
              deriving (Show, Eq)


-- ** Shader
-- | [String] A program name. This name will be shown in error messages and used as binary cache key.
-- 
-- [Shaders] One or more 'VertexShader' and 'FragmentShader' can be specified.
-- Note: Only 1 main() is allowed for each type of shaders.
data Program =
	Program String [Shader] !ProgramRef [ShaderRef]
	deriving (Show, Eq)
	-- GLManager type: add setAPIVersion, setCompiledProgs[(name,blob)], getAllProgramBinaries

-- | [String] Name of the shader.
-- [ByteString] Shader's source code. Encoding is UTF-8 (ES 3.0+) or ASCII (2.0).
data Shader =
	  VertexShader String B.ByteString
	| FragmentShader String B.ByteString
	-- .| ComputeShader String B.ByteString
	deriving (Show, Eq)


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
	deriving (Show, Eq)

data Capability =
	  CullFaceTest
	| Blend -- ^ Applies to all draw buffers
	| Dither
	| StencilTest
	| DepthTest
	| ScissorTest
	| PolygonOffsetFill
	| SampleAlphaToCoverage
	| SampleCoverage
	| PrimitiveRestartFixedIndex -- ^ introduced in ES 3.0
	| RasterizerDiscard -- ^ introduced in ES 3.0
	| SampleMask -- ^ introduced in ES 3.1
	deriving (Show, Eq)

data CullFace = Front | Back | FrontAndBack
	deriving (Show, Eq)

data CompFunc =
	  Never | Less | Equal | LessEq | Greater
	| NotEq | GreatEq | Always
	deriving (Show, Eq)

data StencilOp =
	  OpZero | OpKeep | OpReplace | OpIncr
	| OpDecr | OpInvert | OpIncrWrap | OpDecrWrap
	deriving (Show, Eq)

data BlendOp = Add | Sub | ReverseSub
	deriving (Show, Eq)

data BlendingFactor =
	  FactorZero | FactorOne
	| SrcColor | OneMinusSrcColor
	| DstColor | OneMinusDstColor
	| SrcAlpha | OneMinusSrcAlpha
	| DstAlpha | OneMinusDstAlpha
	| ConstColor | OneMinusConstColor
	| ConstAlpha | OneMinusConstAlpha
	| SrcAlphaSaturate -- ^ src only
	deriving (Show, Eq)

data Hint = DontCare | Fastest | Nicest
	deriving (Show, Eq)


-- ** Vertex Attribute

-- | [String] Variable name to bind the vertex attribute
data VertexAttr =
	  Vertex String VertexData
	-- ^ for vec[1234] Floating integer data per vertex
	| NormalizedVertex String VertexData
	-- ^ for vec[1234] Clamp values to [0,1] ([-1,1] if signed)
	| IntVertex String VertexData
	-- ^ for ivec[1234] Integer variant of Vertex
	| Instanced !Word32 !VertexAttr
	-- ^ Treat as: let vertexList' = concatMap (replicate instanceNum) vertexList
	| BufferSlice String !BufferRef !GLint !GLenum !GLboolean !GLsizei !Int
	-- ^ Internally used. Wrapping glVertexAttribPointer()
	| BufferSlicei String !BufferRef !GLint !GLenum !GLsizei !Int
	-- ^ Internally used. Wrapping glVertexAttribIPointer()
	| ConstAttr1f String !Float
	| ConstAttr2f String !Vec2
	| ConstAttr3f String !Vec3
	| ConstAttr4f String !Vec4
	| ConstAttr4i String !Int32 !Int32 !Int32 !Int32 -- ^ introduced in ES 3.0
	| ConstAttr4ui String !Word32 !Word32 !Word32 !Word32 -- ^ introduced in ES 3.0
	deriving (Show, Eq)

attrVarName :: VertexAttr -> String
attrVarName x = case x of
	Vertex s _ -> s; NormalizedVertex s _ -> s;
	IntVertex s _ -> s; Instanced _ s -> attrVarName s;
	BufferSlice s _ _ _ _ _ _ -> s;
	BufferSlicei s _ _ _ _ _ -> s;
	ConstAttr1f s _ -> s; ConstAttr2f s _ -> s;
	ConstAttr3f s _ -> s; ConstAttr4f s _ -> s;
	ConstAttr4i s _ _ _ _ -> s; ConstAttr4ui s _ _ _ _ -> s;

-- | Vector array
-- 
-- [Int] Dimentions of the vector
data VertexData =
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
	| NoneV -- ^ Do nothing. For early development.
	deriving (Show, Eq)


-- ** Uniform Variable

data UniformVar = UniformVar
	{ uniformName :: String
	, uniformValue :: !UniformValue
	, uniformId :: !UniformId
	}
	deriving (Show, Eq)

-- | bvec[1-4] <- [1-4]{f,i,ui}v?, sampler <- 1iv?
-- Matrices are /row-major/ by default against OpenGL standard.
data UniformValue =
	  Uniform1f !Float
	| Uniform2f !Vec2
	| Uniform3f !Vec3
	| Uniform4f !Vec4
	| Uniform1fv ![Float]
	| Uniform2fv ![Vec2]
	| Uniform3fv ![Vec3]
	| Uniform4fv ![Vec4]
	| Uniform1i !Int32
	| Uniform2i !(Int32, Int32) -- should make sth like IVec2?
	| Uniform3i !(Int32, Int32, Int32)
	| Uniform4i !(Int32, Int32, Int32, Int32)
	| Uniform1iv ![Int32]
	| Uniform2iv ![(Int32, Int32)]
	| Uniform3iv ![(Int32, Int32, Int32)]
	| Uniform4iv ![(Int32, Int32, Int32, Int32)]
	| UniformMat2 !Mat2
	| UniformMat3 !Mat3
	| UniformMat4 !Mat4
	| UniformMat2v ![Mat2]
	| UniformMat3v ![Mat3]
	| UniformMat4v ![Mat4]
	| UniformTexture !Texture
	| UniformTextures ![Texture]
	-- ES 3.0
	| Uniform1ui !Word32
	| Uniform2ui !(Word32, Word32)
	| Uniform3ui !(Word32, Word32, Word32)
	| Uniform4ui !(Word32, Word32, Word32, Word32)
	| Uniform1uiv ![Word32]
	| Uniform2uiv ![(Word32, Word32)]
	| Uniform3uiv ![(Word32, Word32, Word32)]
	| Uniform4uiv ![(Word32, Word32, Word32, Word32)]
	| UniformMat2x3 !(Vec2, Vec2, Vec2) -- not sure these are good
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
	deriving (Show, Eq)

deriving instance Eq Vec2
deriving instance Eq Vec3
deriving instance Eq Vec4
deriving instance Eq Mat2
deriving instance Eq Mat3
deriving instance Eq Mat4


-- ** Texture

data Texture =
	  Texture
		{ texName :: String
		, texTarget :: TextureTarget
		, texColorFormat :: TextureColorFormat
		, texBitLayout :: TextureBitLayout
		, texInternalFormat :: TextureInternalFormat
		, texSampler :: Sampler
		, texWidth :: Int
		, texHeight :: Int
		, texBorder :: Int
		, texLevel :: Int -- [Int]?
		-- texUnit :: Maybe Int 0..31
		}
	| Texture'
		{ -- add width,height,... good for debug?
		  texName :: String
		, texSampler :: Sampler
		, texRef :: TextureRef
		-- texUnit :: Int
		}
	deriving (Show, Eq)

data TextureTarget =
	  Tex2D
	| CubeMapPositiveX
	| CubeMapPositiveY
	| CubeMapPositiveZ
	| CubeMapNegativeX
	| CubeMapNegativeY
	| CubeMapNegativeZ
	| Tex3D -- ^ introduced in ES 3.0
	| Tex2DArray -- ^ introduced in ES 3.0
	deriving (Show, Eq)

data TextureColorFormat = ALPHA | RGB | RGBA | LUMINANCE | LUMINANCE_ALPHA
	deriving (Show, Eq)

data TextureBitLayout = UByte | US565 | US444 | US5551
	-- | ES 3.0
	| Byte | UShort | Short | UInt | Int | HalfFloat | Float | US4444 | UI2_10_10_10Rev | UI24_8 | UI_10f11f11fRev | UI5999Rev | F32UI24_8Rev
	deriving (Show, Eq)

data TextureInternalFormat = Alpha | Rgb | Rgba | Luminance | LuminanceAlpha
	-- 3.0
	| R8 | R8i | R8ui | R8snorm | R16i | R16ui | R16f | R32i | R32ui | R32f
	| Rg8 | Rg8i | Rg8ui | Rg8snorm | Rg16i | Rg16ui | Rg16f | Rg32i | Rg32ui | Rg32f
	| Rgb8 | Rgb8i | Rgb8ui | Rgb8snorm | Rgb16i | Rgb16ui | Rgb16f | Rgb32i | Rgb32ui | Rgb32f
	| Rgba8 | Rgba8i | Rgba8ui | Rgba8snorm | Rgba16i | Rgba16ui | Rgba16f | Rgba32i | Rgba32ui | Rgba32f
	| Rgb5a1 | Rgb565 | Rgb9e5 | Rgb10a2 | Rgb10a2ui | Srgb8 | Rgba4 | Srgb8Alpha8
	| R11fG11fB10f | DepthComponent16 | DepthComponent24 | DepthComponent32
	| Depth24Stencil8 | Depth32fStencil8
	deriving (Show, Eq)

data TextureData =
	  RawTexture
	| PVRTC -- PowerVR SGX(iOS), Samsung S5PC110(Galaxy S/Tab)
	| ATITC -- ATI Imageon, Qualcomm Adreno
	| S3TC -- NVIDIA Tegra2, ZiiLabs ZMS-08 HD
	| ETC1 -- Android, ARM Mali
	-- .| _3Dc -- ATI, NVidia
	-- .| Palette

-- ** Sampler

data Sampler = Sampler2D
		{ magFilter :: !MagFilter
		, minFilter :: !MinFilter
		, wrapS :: !WrapMode
		, wrapT :: !WrapMode
		}
		-- 2
	-- .| SamplerES3
	deriving (Show, Eq)

data MagFilter = MagNearest | MagLinear
	deriving (Show, Eq)

data MinFilter = MinNearest | MinLinear
	| NearestMipmapNearest | NearestMipmapLinear
	| LinearMipmapNearest | LinearMipmapLinear
	deriving (Show, Eq)

data WrapMode = Repeat | ClampToEdge | MirroredRepeat
	deriving (Show, Eq)


-- ** Vertex Picker

-- | VFromCounts -> VFromCounts', VIndex8/16/32 -> VIndex(Instanced)',
-- VIndices8/16/32 -> VIndices' or DrawCallSequence[VIndex(Instanced)']
--
-- Version/Ext fallback feature is not yet. (See above)
data VertexPicker =
	  VFromCount !Int32 !Word32
	| VFromCountInstanced !Int32 !Word32 !Word32
	| VFromCounts ![(Int32, Word32)]
	| VFromCounts' !B.ByteString !B.ByteString
	-- ^ Wrapping glMultiDrawArraysEXT
	| VIndex8 ![Word8]
	| VIndex16 ![Word16]
	| VIndex32 ![Word32]
	| VIndex' !BufferRef !GLsizei !GLenum !Int
	-- ^ Wrapping glDrawElements
	| VIndexInstanced8 ![Word8] !Int
	| VIndexInstanced16 ![Word16] !Int
	| VIndexInstanced32 ![Word32] !Int
	| VIndexInstanced' !BufferRef !GLsizei !GLenum !Int !GLsizei
	-- ^ Wrapping glDrawElementsInstanced
	| VIndices8 ![[Word8]]
	| VIndices16 ![[Word16]]
	| VIndices32 ![[Word32]]
	| VIndices' !B.ByteString GLenum !B.ByteString
	-- ^ Wrapping glMultiDrawElementsEXT
	-- .| VFromToIndex8/16/32 !Int !Int ![Word8/16/32]
	-- .| VFromToIndex' !BufferRef !Int !Int !GLsizei !GLenum !Int
	-- .^ Wrapping glDrawRangeElements
	| DrawCallSequence ![VertexPicker]
	deriving (Show, Eq)

-- * Data-driven Rendering

-- ** Initialization

data GLManager = GLManager
	{ glAPIVersion :: GLVersion -- ^ set API version to be used
	, compiledProgramCache :: IORef [(String, B.ByteString)]
	-- ^ get/set after/before program linkage
	--, glRefHolder :: [ForeignPtr GLuint]
	} deriving Show
instance Show (IORef a) where show = const "(IORef ..)"

data GLVersion = ES2 | ES3 | ES31 deriving Show

initGLManager :: IO GLManager
initGLManager = newIORef [] >>= return . GLManager ES2 

-- ** Draw
-- | Allocate video memory for rendering in advance and compile a call
-- that optimised for the running environment.
compileCall :: GLManager -- ^ API version and program binary caches
            -> DrawCall -- ^ DrawCall to compile
            -> IO (Either [String] DrawCall) -- ^ Return errors or compiled call
compileCall glm@(GLManager version cache)
	-- TODO split into small compilers
	(DrawUnit mode prog conf uniforms attribs picker) = do

	let Program progName shaders _ _ = prog nullFPtr []
	caches <- readIORef cache
	program <- case lookup progName caches of
		Just bs -> do
			pid <- loadProgramBinary progName bs
			case pid of
				Just pid -> do
					fpid <- bindFinalizer (glDeleteProgram pid) pid
					return . Right $ prog fpid []
				Nothing -> return $ Left ["Broken program binary cache: " ++ progName]
		Nothing -> do
			either <- loadProgram glm progName shaders
			eitherIO either $ \(x : xs) -> do
				-- TODO create cache here
				p <- bindFinalizer (glDeleteProgram x) x
				s <- mapM (\x -> bindFinalizer (glDeleteShader x) x) xs
				return . Right $ prog p s
	
	d <- eitherIO program $ \prog@(Program _ _ fp _) -> do
		pid <- withForeignPtr fp (return . ptrToId)
		attr' <- forM attribs $ \attr -> do
			let name = attrVarName attr
			i <- getAttribLocation pid name
			showError $ "glGetAttribLocation " ++ show (progName, name, i)
			if i < 0 then error $ "Vertex attribute '" ++ name
				++ "' was not found in shader program '" ++ progName
				++ "'"
			else return $ (fromIntegral i, attr)
		
		unifs <- forM uniforms $ \u -> do
			let name = uniformName $ u 0
			i <- getUniformLocation pid name
			showError $ "glGetUniformLocation" ++ show (progName, name, i)
			--if i < 0 then error $ "Uniform variable '" ++ name
			--	++ "' was not found in shader program '" ++ progName
			--	++ "'"
			--else
			return $ u i

		attrs <- mapM compileVertexAttr attr'
		
		-- unifs <- foldr compileTexture (-1) unifs'
		-- call glPixelStorei GL_[UN]PACK_ALIGNMENT [1248] before texImage2d
		
		picker' <- compilePicker False picker
		return . Right $ DrawCall mode prog conf unifs attrs picker'

	return d

compileCall :: GLManager -> DrawCall -> IO DrawCall
compileCall' glm dc = compileCall glm dc >>= \x -> case x of
	Left errors -> error (show errors)
	Right dc -> return dc


eitherIO :: (Monad m) => Either a b -> (b -> m (Either a c)) -> m (Either a c)
eitherIO (Right r) f = f r
eitherIO (Left l) _ = return (Left l)

drawData :: DrawCall -> IO ()
drawData (DrawCall mode prog conf uniforms attribs picker) = do
	-- draw config
	mapM_ setGraphicState conf

	-- shader setting
	let Program _ _ progRef _ = prog
	withForeignPtr progRef (glUseProgram . ptrToId)
	
	-- vertex attribute
	mapM_ setVertexAttr attribs
	
	-- uniform variable
	mapM_ setUniformVar uniforms
	
	-- texture
	-- glActiveTexture(0-31)
	-- glBindTexture 2D texRef
	-- glBindSampler ...
	-- glUniform1i unifid 0-31
	
	invokeDraw mode picker

drawData (DrawTexture ref u v tw th x y z w h) = do
	-- GL_TEXTURE_2D = 0x0DE1, GL_TEXTURE_CROP_RECT_OES = 0x8B9D
	withForeignPtr ref (glBindTexture 0x0DE1 . ptrToId)
	withArray [u, v, tw, th] (glTexParameteriv 0x0DE1 0x8B9D)
	glDrawTexiOES x y z w h -- disable AlphaTest?


-- * Internals

-- ** Garbage collection for GPU objects

bindFinalizer :: IO () -> ResourceId -> IO (ForeignPtr ResourceId)
bindFinalizer f i = newForeignPtr (idToPtr i) (putStrLn "Fin" >> f)

idToPtr i = nullPtr `plusPtr` fromIntegral i
ptrToId ptr = fromIntegral $ ptr `minusPtr` nullPtr
finalize = finalizeForeignPtr


-- ** Error
data GLError = NoError | InvalidEnum | InvalidValue | InvalidOperation
             | OutOfMemory | InvalidFrameBufferOperation deriving Show

getError :: IO GLError
getError = unMarshal <$> glGetError
	where unMarshal x = case x of
		0x0000 -> NoError
		0x0500 -> InvalidEnum
		0x0501 -> InvalidValue
		0x0502 -> InvalidOperation
		0x0505 -> OutOfMemory
		0x0506 -> InvalidFrameBufferOperation

showError :: String -> IO ()
showError location = do
	getError >>= \err -> case err of
		NoError -> return ()
		_ -> putStrLn $ "GLError " ++ location ++ ": " ++ show err


-- ** Compiling

instance Marshal DrawMode where
	marshal x = case x of
		Points -> 0
		Lines -> 1
		LineLoop -> 2
		LineStrip -> 3
		Triangles -> 4
		TriangleStrip -> 5
		TriangleFan -> 6

data ProgramProps = DeleteStatus
                  | LinkStatus
                  | ValidateStatus
                  | ProgramInfoLogLength
                  | AttachedShaders
                  | ActiveAttributes
                  | ActiveAttributeMaxLength
                  | ActiveUniforms
                  | ActiveUniformMaxLength
                  -- and more and more on ES 3.0

instance Marshal ProgramProps where
	marshal x = case x of
		DeleteStatus -> 0x8B80
		LinkStatus -> 0x8B82
		ValidateStatus -> 0x8B83
		ProgramInfoLogLength -> 0x8B84
		AttachedShaders -> 0x8B85
		ActiveAttributes -> 0x8B89
		ActiveUniformMaxLength -> 0x8B8A
		-- ......

loadProgram :: GLManager -> String -> [Shader] -> IO (Either [String] [ResourceId])
loadProgram glm progname shaders = do
	results <- mapM loadShader shaders
	-- putStrLn $ show results
	let (errors, resids) = partitionEithers results
	if errors /= [] then return $ Left errors
	else do
		pid <- glCreateProgram
		if pid == 0 then do
			showError "glCreateProgram"
			return (Left ["glCreateProgram returned 0."])
		else do
			forM_ resids (\sid ->
				glAttachShader pid sid >> showError "glAttachShader")
			glLinkProgram pid
			alloca $ \pint -> do
				glGetProgramiv pid (marshal LinkStatus) pint
				linkStatus <- peek pint
				if linkStatus == 0 then do
					glGetProgramiv pid (marshal ProgramInfoLogLength) pint
					len <- peek pint
					msg <- allocaBytes (fromIntegral len) $ \buf -> do
						glGetProgramInfoLog pid (fromIntegral len) nullPtr buf
						msg <- peekCStringLen (buf, fromIntegral len)
						return $ "Cannot link program " ++ progname ++ "\n" ++ msg
					putStrLn msg
					glDeleteProgram pid
					return (Left [msg])
				else return $ Right (pid : resids)

loadProgramBinary :: String -> B.ByteString -> IO (Maybe ResourceId)
loadProgramBinary progName bs = do
	pid <- glCreateProgram
	if pid == 0 then do
		showError "glCreateProgram"
		return Nothing
	else do
		let (fp, offset, len) = toForeignPtr bs
		withForeignPtr fp $ \p -> do
			fmt <- peek (p `plusPtr` offset)
			glProgramBinary pid fmt (p `plusPtr` (offset+4)) (fromIntegral len)
		alloca $ \pint -> do
			glGetProgramiv pid (marshal LinkStatus) pint
			linkStatus <- peek pint
			if linkStatus == 0 then do
				glDeleteProgram pid >> return Nothing
			else return $ Just pid
		
data ShaderProps = ShaderType
                 | CompileStatus
                 | ShaderInfoLogLength
                 | ShaderSourceLength
                 | ShaderCompiler
                 -- and more on ES 3.0

instance Marshal ShaderProps where
	marshal x = case x of
		ShaderType -> 0x8B4F
		CompileStatus -> 0x8B81
		ShaderInfoLogLength -> 0x8B84
		ShaderSourceLength -> 0x8B88
		ShaderCompiler -> 0x8DFA
		-- ...

loadShader :: Shader -> IO (Either String ResourceId)
loadShader s =
	case s of
		VertexShader name bs -> go 0x8B31 name bs
		FragmentShader name bs -> go 0x8B30 name bs
	where go shaderType name blob = do
		sid <- glCreateShader shaderType
		if sid == 0 then do
			showError "glCreateShader"
			return . Left $ "glCreateShader returned 0."
		else do
			B.useAsCString blob $ \src -> do
				withArray [src] $ \ptr -> do
					glShaderSource sid 1 ptr nullPtr
					glCompileShader sid
					alloca $ \pint -> do
						glGetShaderiv sid (marshal CompileStatus) pint
						compiled <- peek pint
						if compiled == 0 then do
							glGetShaderiv sid (marshal ShaderInfoLogLength) pint
							len <- peek pint
							msg <- allocaBytes (fromIntegral len) $ \buf -> do
								glGetShaderInfoLog sid (fromIntegral len) nullPtr buf
								msg <- peekCStringLen (buf, fromIntegral len)
								return $ "Could not compile " ++ " " ++ name ++ "\n" ++ msg
							putStrLn msg
							glDeleteShader sid
							return (Left msg)
						else return $ Right sid

getAttribLocation :: ResourceId -> String -> IO GLint
getAttribLocation prog name =
	withCString name (glGetAttribLocation prog)

getUniformLocation :: ResourceId -> String -> IO UniformId
getUniformLocation prog name =
	withCString name (glGetUniformLocation prog)

compileVertexAttr :: (AttrId, VertexAttr) -> IO (AttrId, VertexAttr)
compileVertexAttr self@(loc, va) = case va of
	Vertex name vect -> compileVertex name vect loc 0
	NormalizedVertex name vect -> compileVertex name vect loc 1
	IntVertex name vect ->
		return self -- XXX go name vect loc BufferSlicei
	Instanced divNum va -> do
		(_, compiled) <- compileVertexAttr (0, va)
		return $ (loc, Instanced divNum compiled)
		-- XXX fallback
	--BufferSlice _ ref size typ normalize stride offset loc -> do	
	--BufferSlicei _ ref size typ stride offset loc -> do
	-- ConstAttr[1234]f/4i/4ui
	_ -> return self

gl_array_buffer = 0x8892
gl_static_draw = 0x88E4

compileVertex name NoneV loc _ = return $ (loc, Vertex name NoneV)
compileVertex name (BlobSliced vd bs) loc normalize = do
	bidptr <- malloc
	bid <- glGenBuffers 1 bidptr >> peek bidptr
	showError "glGenBuffers"
	putStrLn $ show bid
	glBindBuffer gl_array_buffer bid
	showError "glBindBuffer"

	let (vsize, dim, typ, _) = vertexFormat vd
	if dim < 1 || 4 < dim
		then error $ "compileVertex: Unacceptable dimentions found in " ++ show name
		else return ()
	let (fp, offset, len) = toForeignPtr bs
	xslen <- withForeignPtr fp $ \ptr -> do
		glBufferData gl_array_buffer (fromIntegral len)
		             (castPtr ptr `plusPtr` offset) gl_static_draw
		showError "glBufferData"
		return $ fromIntegral len
	fpb <- bindFinalizer (glDeleteBuffers 1 bidptr >> free bidptr) bid
	return $ (loc, BufferSlice name fpb (xslen `div` dim) typ normalize
		(vsize * fromIntegral dim) 0)
compileVertex name vect loc normalize = do
	bidptr <- malloc
	bid <- glGenBuffers 1 bidptr >> peek bidptr
	showError "glGenBuffers"
	putStrLn $ show bid
	glBindBuffer gl_array_buffer bid
	showError "glBindBuffer"

	let (vsize, dim, typ, withArrayLen_xs) = vertexFormat vect
	if dim < 1 || 4 < dim
		then error $ "compileVertex: Unacceptable dimentions found in " ++ show name
		else return ()
	xslen <- withArrayLen_xs $ \len ptr -> do
		glBufferData gl_array_buffer (fromIntegral len * fromIntegral vsize)
		             (castPtr ptr) gl_static_draw
		showError "glBufferData"
		return $ fromIntegral len
	fpb <- bindFinalizer (glDeleteBuffers 1 bidptr >> free bidptr) bid
	return $ (loc, BufferSlice name fpb (xslen `div` dim) typ normalize
		(vsize * fromIntegral dim) 0)

withArrayLen' :: (Storable a) => [a] -> (Int -> Ptr b -> IO c) -> IO c
withArrayLen' xs f = withArrayLen xs $ \l p -> f l (castPtr p)

vertexFormat v = case v of
	ByteV n xs -> (1, n, 0x1400, withArrayLen' xs) -- GL_BYTE
	UByteV n xs -> (1, n, 0x1401, withArrayLen' xs) -- GL_UNSIGNED_UBYTE
	ShortV n xs -> (2, n, 0x1402, withArrayLen' xs) -- GL_SHORT
	UShortV n xs -> (2, n, 0x1403, withArrayLen' xs) -- GL_UNSIGNED_SHORT
	IntV n xs -> (4, n, 0x1404, withArrayLen' xs) -- GL_INT
	UIntV n xs -> (4, n, 0x1405, withArrayLen' xs) -- GL_UNSIGNED_INT
	FloatV n xs -> (4, n, 0x1406, withArrayLen' xs) -- GL_FLOAT
	HalfFloatV n xs -> (2, n, 0x140B, withArrayLen' xs) -- GL_HALF_FLOAT
	FixedV n xs -> (4, n, 0x140C, withArrayLen' xs) -- GL_FIXED
	I2_10_10_10V xs -> (4, 1, 0x8D9F, withArrayLen' xs) -- GL_INT_2_10_10_10_REV
	UI2_10_10_10V xs -> (4, 1, 0x8368, withArrayLen' xs) -- GL_UNSIGNED_INT_2_10_10_10_REV

newBuffer proc = do
	bidptr <- malloc
	bid <- glGenBuffers 1 bidptr >> peek bidptr
	showError "glGenBuffers"
	putStrLn $ show bid
	glBindBuffer gl_array_buffer bid
	showError "glBindBuffer"
	result <- proc
	fpb <- bindFinalizer (glDeleteBuffers 1 bidptr >> free bidptr) bid
	return (result, fpb)

compilePicker :: Bool -> VertexPicker -> IO VertexPicker
compilePicker instanced vp = case vp of
	VFromCount first count -> return vp
	VFromCountInstanced first count divNum -> return vp
	VFromCounts list -> do
		let (firsts, counts) = unzip list
		firstbs <- create (length list * 4) (flip pokeArray firsts . castPtr)
		countbs <- create (length list * 4) (flip pokeArray counts . castPtr)
		return $ VFromCounts' firstbs countbs
	VFromCounts' firsts counts -> return vp
	VIndex8 index -> vindexN (withArrayLen' index) 1 0x1401 -- GL_UNSIGNED_BYTE
	VIndex16 index -> vindexN (withArrayLen' index) 2 0x1403 -- GL_UNSIGNED_SHORT
	VIndex32 index -> vindexN (withArrayLen' index) 4 0x1405 -- GL_UNSIGNED_INT
	VIndex' bref count typ offset -> return vp -- glDrawElements will be called
	VIndexInstanced8 index divNum -> return vp -- XXX
	VIndexInstanced16 index divNum -> return vp -- XXX
	VIndexInstanced32 index divNum -> return vp -- XXX
	VIndexInstanced' bref count typ offset divNum -> return vp
	VIndices8 indices ->
		DrawCallSequence <$> mapM (compilePicker False . VIndex8) indices
		-- XXX if glMultiDrawElements is available, compile to VIndices'
		--let counts = map (size.length) indices
		--let index = concat indices
		--countbs <- create (length indices * 4) (pokeArray counts . castPtr)
		--indexbs <- create (sum counts * 1?2?4) (pokeArray index . castPtr)
		--VIndices' countbs  indexbs
	VIndices16 indices ->
		DrawCallSequence <$> mapM (compilePicker False . VIndex16) indices
	VIndices32 indices ->
		DrawCallSequence <$> mapM (compilePicker False . VIndex32) indices
	VIndices' counts typ indices -> return vp
	-- VFromToIndex8/16/32 !Int !Int ![Word8/16/32]
	-- VFromToIndex' !BufferRef !Int !Int !GLsizei !GLenum !Int
	DrawCallSequence xs ->
		DrawCallSequence <$> mapM (compilePicker instanced) xs
	where
		vindexN withArrayLen_ix size typ = do
			(count, buffer) <- newBuffer $ do
				withArrayLen_ix $ \count ptr -> do
					glBufferData gl_array_buffer (fromIntegral count * size)
					    ptr gl_static_draw
					showError "glBufferData"
					return $ fromIntegral count
			return $ VIndex' buffer count typ 0

-- ** Drawing

instance Marshal CullFace where
	marshal Front        = 0x0404
	marshal Back         = 0x0405
	marshal FrontAndBack = 0x0408

instance Marshal CompFunc where
	marshal x = case x of
		Never    -> 0x0200
		Less     -> 0x0201
		Equal    -> 0x0202
		LessEq   -> 0x0203
		Greater  -> 0x0204
		NotEq    -> 0x0205
		GreatEq  -> 0x0206
		Always   -> 0x0207

instance Marshal StencilOp where
	marshal x = case x of
		OpZero     -> 0x0000
		OpKeep     -> 0x1E00
		OpReplace  -> 0x1E01
		OpIncr     -> 0x1E02
		OpDecr     -> 0x1E03
		OpInvert   -> 0x150A
		OpIncrWrap -> 0x8507
		OpDecrWrap -> 0x8508

instance Marshal BlendOp where
	marshal Add        = 0x8006
	marshal Sub        = 0x800A
	marshal ReverseSub = 0x800B

instance Marshal BlendingFactor where
	marshal x = case x of
		FactorZero         -> 0
		FactorOne          -> 1
		SrcColor           -> 0x300
		OneMinusSrcColor   -> 0x301
		SrcAlpha           -> 0x302
		OneMinusSrcAlpha   -> 0x303
		DstAlpha           -> 0x304
		OneMinusDstAlpha   -> 0x305
		DstColor           -> 0x306
		OneMinusDstColor   -> 0x307
		SrcAlphaSaturate   -> 0x308
		ConstColor         -> 0x8001
		OneMinusConstColor -> 0x8002
		ConstAlpha         -> 0x8003
		OneMinusConstAlpha -> 0x8004

instance Marshal Hint where
	marshal DontCare = 0x1100
	marshal Fastest  = 0x1101
	marshal Nicest   = 0x1102

setGraphicState :: GraphicState -> IO ()
setGraphicState x = case x of
	Begin cap -> glEnable (marshal cap)
	End cap -> glDisable (marshal cap)
	LineWidth width -> glLineWidth width
	FrontFace cw -> glFrontFace (if cw then 0x900 else 0x901)
	CullFace cf -> glCullFace (marshal cf)
	PolygonOffset factor units -> glPolygonOffset factor units
	Scissor l b w h -> glScissor l b w h
	SampleCvrg value invert ->
		glSampleCoverage value (if invert then 1 else 0)
	StencilFunc func comp mask ->
		glStencilFunc (marshal func) comp mask
	StencilFuncSeparate cull f c m ->
		glStencilFuncSeparate (marshal cull) (marshal f) c m
	StencilOp sfail dpfail dppass ->
		glStencilOp (marshal sfail) (marshal dpfail) (marshal dppass)
	StencilOpSeparate cull sfail dpfail dppass ->
		glStencilOpSeparate (marshal cull) (marshal sfail)
			(marshal dpfail) (marshal dppass)
	DepthFunc comp -> glDepthFunc (marshal comp)
	BlendEquation mode -> glBlendEquation (marshal mode)
	BlendEquationSeparate rgb a ->
		glBlendEquationSeparate (marshal rgb) (marshal a)
	BlendFunc src dest -> glBlendFunc (marshal src) (marshal dest)
	BlendFuncSeparate srgb drgb salpha dalpha ->
		glBlendFuncSeparate (marshal srgb) (marshal drgb)
			(marshal salpha) (marshal dalpha)
	BlendColor r g b a -> glBlendColor r g b a
	GenerateMipmapHint hint -> glHint 0x8192 (marshal hint)
	FragmentShaderDerivativeHint hint -> glHint 0x8B8B (marshal hint)

setVertexAttr :: (AttrId, VertexAttr) -> IO ()
setVertexAttr (loc, va) = case va of
	Vertex _ va -> error "setVertexAttr: VertexAttr must be compiled!"
	NormalizedVertex _ va -> error "setVertexAttr: VertexAttr must be compiled!"
	IntVertex _ va -> error "setVertexAttr: VertexAttr must be compiled!"
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
		d = glDisableVertexAttribArray loc


setUniformVar :: UniformVar -> IO ()
setUniformVar (UniformVar _ val loc) =
	case val of
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
		--UniformTexture x -> glUniform1i loc x
		--UniformTextures xs -> withArray xs (glUniform1iv loc (len xs))
		Uniform1ui x -> glUniform1ui loc x
		Uniform2ui (x,y) -> glUniform2ui loc x y
		Uniform3ui (x,y,z) -> glUniform3ui loc x y z
		Uniform4ui (x,y,z,w) -> glUniform4ui loc x y z w
		{-Uniform1uiv xs -> withArray xs (glUniform1uiv loc (len xs))
		Uniform2uiv xs -> withArray xs (glUniform2uiv loc (len xs))
		Uniform3uiv xs -> withArray xs (glUniform3uiv loc (len xs))
		Uniform4uiv xs -> withArray xs (glUniform4uiv loc (len xs))-}
	where
		len = fromIntegral . length
	{-- ES 3.0
	| UniformMat2x3 !(Vec2, Vec2, Vec2) -- not sure these are good
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


instance Marshal Capability where
	marshal x = case x of
		CullFaceTest          -> 0x0B44
		Blend                 -> 0x0BE2
		Dither                -> 0x0BD0
		StencilTest           -> 0x0B90
		DepthTest             -> 0x0B71
		ScissorTest           -> 0x0C11
		PolygonOffsetFill     -> 0x8037
		SampleAlphaToCoverage -> 0x809E
		SampleCoverage        -> 0x80A0
		PrimitiveRestartFixedIndex -> 0x8D69
		RasterizerDiscard     -> 0x8C89
	-- SampleMask -> 0x

withBSBS :: B.ByteString -> B.ByteString
         -> (Ptr a -> Ptr b -> Word32 -> IO c)
         -> IO c
withBSBS bs1 bs2 f =
	withForeignPtr fp1 $ \p1 ->
		withForeignPtr fp2 $ \p2 ->
			f (castPtr $ p1 `plusPtr` off1)
			  (castPtr $ p2 `plusPtr` off2) (fromIntegral len)
	where
		(fp1,off1,len) = toForeignPtr bs1
		(fp2,off2,_) = toForeignPtr bs2

invokeDraw :: DrawMode -> VertexPicker -> IO ()
invokeDraw mode picker = case picker of
	VFromCount first count -> do
		glDrawArrays m first count
		showError "glDrawArrays"
	VFromCountInstanced first count primcount ->
		glDrawArraysInstanced m first count primcount
	VFromCounts list ->
		forM_ list (\(fst, cnt) -> glDrawArrays m fst cnt)
	VFromCounts' firstbs countbs ->
		withBSBS countbs firstbs $ \cptr fptr clen ->
			glMultiDrawArraysEXT m fptr cptr (unsafeShiftR clen 2)
	VIndex' ref count typ offset -> do
		-- bind GL_ELEMENT_ARRAY_BUFFER = 0x8893
		withForeignPtr ref (glBindBuffer 0x8893 . ptrToId)
		glDrawElements m count typ (idToPtr offset)
	VIndexInstanced' ref count typ offset divNum ->
		glDrawElementsInstanced m count typ (idToPtr offset) divNum
	--VIndices8/16/32 ... ->
	VIndices' countbs typ indicesbs ->
		withBSBS countbs indicesbs $ \cptr iptr clen ->
			glMultiDrawElementsEXT m cptr typ iptr (unsafeShiftR clen 2)
	-- VFromToIndex' ...
	DrawCallSequence xs -> mapM_ (invokeDraw mode) xs
	_ -> error $ "invokeDraw: VertexPicker (" ++ show picker ++ ") must be compiled."
	where
		m = marshal mode