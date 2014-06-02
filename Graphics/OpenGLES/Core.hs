{-# LANGUAGE FlexibleInstances #-}
module Graphics.OpenGLES.Core where
import qualified Data.ByteString as B
import Control.Applicative
import Foreign
import Foreign.C.String
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import Graphics.OpenGLES.Base

foreign import ccall "wrapper"
	wrapFinalizerPtr :: (Ptr () -> IO ()) -> IO (FinalizerPtr ())

bindFinalizer :: (GLuint -> IO ()) -> GLuint -> IO (ForeignPtr a)
bindFinalizer finalizer i = do
	f <- wrapFinalizerPtr (\ptr ->
		finalizer (fromIntegral $ ptr `minusPtr` nullPtr))
	let idptr = nullPtr `plusPtr` fromIntegral i
	newForeignPtr f idptr

--program <- bindFinalizer glDeleteProgram programId

data GLManager = GLManager
	{ glAPIVersion :: GLVersion -- ^ specify API to be used
	, compiledProgramBinaries :: [(String, B.ByteString)]
	-- ^ get/set after/before program linkage
	, buffers, programs, textures, framebuffers, renderbuffers
	}

data GLVersion = ES20 | ES30 | ES31 deriving Show

-- | Resource Region is used to keep track of video memory allocations,
-- and you should free consumed memory by calling `freeRegion`
-- when you're sure 
data ResourceRegion = RR [Int] [Int] [Int]

-- * Core Data Types

data DrawCall = DrawUnit
		DrawMode
		(ProgramId -> [ShaderId] -> Program)
		Indices
		[AttrId -> VertexAttr]
		[UniformId -> UniformVar]
		[TextureId -> Texture]
		DrawConfig
	| DrawArrays
		{ dcMode :: DrawMode
		, dcProgram :: Program
		, dcCount :: Indices
		, dcAttrs :: [VertexAttr]
		, dcUniforms :: [UniformVar]
		, dcTexture :: [Texture]
		, dcConfig :: DrawConfig
		}
	deriving (Show, Eq)

instance Show (ProgramId -> [ShaderId] -> Program) where show x = show (x 0 [])
instance Eq (ProgramId -> [ShaderId] -> Program) where x == y = x 0 [] == y 0 []

instance Show (AttrId -> VertexAttr) where show x = show (x 0)
instance Eq (AttrId -> VertexAttr) where x == y = x 0 == y 0

instance Show (UniformId -> UniformVar) where show x = show (x 0)
instance Eq (UniformId -> UniformVar) where x == y = x 0 == y 0
 
instance Show (TextureId -> Texture) where show x = show (x 0)
instance Eq (TextureId -> Texture) where x == y = x 0 == y 0
-- XXX: DrawTexture Extension

data Blob =
	  Blob B.ByteString
	| Blob' BufferId
	deriving (Show, Eq)


data DrawMode = Points | Lines | LineLoop | LineStrip
              | Triangles | TriangleStrip | TriangleFan
              deriving (Show, Eq)


-- ** Vertex Indices

type Indices = Int


-- ** Shader

data Program = Program
	{ progName :: String
	, progSdrs :: [Shader]
	, progId :: ProgramId
	, progSids :: [ShaderId]
	}
	deriving (Show, Eq)
	-- XXX get/use binary: Maybe (IO ByteString)
	-- GLManager type: add setAPIVersion, setCompiledProgs[(name,blob)], getAllProgramBinaries

{-data Shader = Shader
	{ s_type :: ShaderType
	, s_source :: Blob
	, s_attribs :: [String]
	, s_uniforms :: [String]
	}
	deriving (Show, Eq)-}
data Shader =
	  VertexShader
		{ sdrName :: String
		, sdrSource :: Blob
		}
	| FragmentShader
		{ sdrName :: String
		, sdrSource :: Blob
		}
	deriving (Show, Eq)


-- ** Vertex Attribute

-- VertexAttr "a_position" F3 FloatT False 0 0 blob
data VertexAttr = VertexAttr
		{ attrVarName :: String
		, attrType :: AttrType
		, attrSrcType :: AttrSrcType
		, attrNormalize :: Bool
		, attrStride :: Int
		, attrOffset :: Int
		, attrArray :: Blob
		, attrId :: AttrId
		}
	| ConstantVA
		{ attrVarName :: String
		, attrValue :: AttrValue
		, attrId :: AttrId
		}
	deriving (Show, Eq)
	--VertexAttrDiv

data AttrType =
	F1 | F2 | F3 | F4 | I1 | I2 | I3 | I4 | UI1 | UI2 | UI3 | UI4
	deriving (Show, Eq)

data AttrSrcType =
	  ByteT
	| UByteT
	| ShortT
	| UShortT
	| FloatT
	| FixedT
	| IntT  -- ^ ES 3.0
	| UIntT -- ^ ES 3.0
	-- more
	deriving (Show, Eq)

data AttrValue =
	  Attr1f GLfloat
	| Attr2f GLfloat GLfloat
	| Attr3f GLfloat GLfloat GLfloat
	| Attr4f GLfloat GLfloat GLfloat GLfloat
	| Attr4i GLint GLint GLint GLint -- ^ ES 3.0
	| Attr4ui GLuint GLuint GLuint GLuint -- ^ ES 3.0
	deriving (Show, Eq)


-- ** Uniform Variable

data UniformVar = UniformVar
	{ uniformName :: String
	, uniformValue :: UniformValue
	, uniformId :: UniformId
	}
	deriving (Show, Eq)

data UniformValue =
	  Uniform1f GLfloat
	| Uniform2f GLfloat GLfloat
	| Uniform3f GLfloat GLfloat GLfloat
	| Uniform4f GLfloat GLfloat GLfloat GLfloat
	| Uniform1i GLint
	| Uniform2i GLint GLint
	| Uniform3i GLint GLint GLint
	| Uniform4i GLint GLint GLint GLint
	| UniformMatrix2 GLfloat GLfloat GLfloat GLfloat
	| UniformMatrix3 GLfloat GLfloat GLfloat GLfloat GLfloat GLfloat GLfloat GLfloat GLfloat
	| UniformMatrix4
	-- ES 3.0
	| Uniform1ui GLuint
	| Uniform2ui GLuint GLuint
	| Uniform3ui GLuint GLuint GLuint
	| Uniform4ui GLuint GLuint GLuint GLuint
	| UniformMatrix2x3
	| UniformMatrix3x2
	| UniformMatrix2x4
	| UniformMatrix4x2
	| UniformMatrix3x4
	| UniformMatrix4x3
	deriving (Show, Eq)


-- ** Texture

data Texture = Texture
	{ texTarget :: TextureTarget
	, texFormat :: TextureFormat
	, texBitLayout :: TextureBitLayout
	, texInternalFormat :: TextureInternalFormat
	, texSampler :: Sampler
	, texWidth :: Int
	, texHeight :: Int
	, texBorder :: Int
	, texLevel :: Int -- [Int]?
	, texId :: TextureId
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
	| Tex3D -- ^ ES 3.0
	| Tex2DArray -- ^ ES 3.0
	deriving (Show, Eq)

data TextureFormat = ALPHA | RGB | RGBA | LUMINANCE | LUMINANCE_ALPHA
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


-- ** Sampler

data Sampler = SamplerES2
		{ magFilter :: MagFilter
		, minFilter :: MinFilter
		, wrapS :: WrapMode
		, wrapT :: WrapMode
		}
		-- 2
	-- | SamplerES3
	deriving (Show, Eq)

data MagFilter = MagNearest | MagLinear
	deriving (Show, Eq)

data MinFilter = MinNearest | MinLinear
	| NearestMipmapNearest | NearestMipmapLinear
	| LinearMipmapNearest | LinearMipmapLinear
	deriving (Show, Eq)

data WrapMode = Repeat | ClampToEdge | MirroredRepeat
	deriving (Show, Eq)


-- ** Graphic State

data DrawConfig = DrawConfig
	{ blendEnable :: Bool
	, cullFaceEnable :: Bool
	, depthMaskEnable :: Bool
	, depthTextEnable :: Bool
	}
	deriving (Show, Eq)


-- * Data-driven Rendering
-- compile,draw,dispose
drawData :: DrawCall -> IO (Either [String] DrawCall)
drawData d@(DrawUnit mode prog count attr uni tex conf) = do
	compiled <- loadProgram prog
	case compiled of
		Left err -> return $ Left err
		Right prog -> do
			attr' <- mapM (\a->
				a <$> getAttribLocation (progId prog) (attrVarName $ a 0)
				) attr
			uni' <- mapM (\u->
				u <$> getUniformLocation (progId prog) (uniformName $ u 0)
				) uni
			-- array buffer
			-- raise if var not found
			let tex' = map ($ 0) tex
			return . Right $ DrawArrays	mode prog count attr' uni' tex' conf
drawData d@(DrawArrays mode prog count attrs vars tex conf) = do
	putStrLn "draw start"
	glUseProgram (progId prog)
	
	-- vertex attribute
	mapM_ setVertexAttr attrs
	-- uniform variable
	mapM_ setUniform vars
	-- texture
	
	let setCapability bool = if bool then enable else disable
	setCapability (blendEnable conf) Blend
	setCapability (cullFaceEnable conf) CullFace
	setCapability (depthTextEnable conf) DepthTest
	glDepthMask (fromBool $ depthMaskEnable conf)

	glDrawArrays (marshal mode) 0 (fromIntegral count)
	return . Right $ d


-- call glPixelStorei GL_[UN]PACK_ALIGNMENT [1248] before texImage2d
-- ** Types
type BufferId = GLuint
type ProgramId = GLuint
type ShaderId = GLuint
type AttrId = GLuint
type UniformId = GLint
type TextureId = GLuint

-- ** Utils
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

-- | fromEnum alternative
class Marshal a where marshal :: (Num n) => a -> n

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
		AttachedShaders -> 0x8B85
		ActiveAttributes -> 0x8B89
		ActiveUniformMaxLength -> 0x8B8A
		-- ......

loadProgram :: (ProgramId -> [ShaderId] -> Program) -> IO (Either [String] Program)
loadProgram arg = do
	let Program name shaders _ _ = arg 0 []
	results <- mapM loadShader shaders	
	let left (Left x) = [x]; left _ = []
	let lefts = concatMap left results
	if lefts /= []
		then return $ Left lefts
	else do
		let right (Right r) = r
		let sdrs = map right results
		pid <- glCreateProgram
		if pid == 0 then do
			showError "glCreateProgram"
			return (Left ["glCreateProgram returned 0."])
		else do
			mapM (\sid ->
				glAttachShader pid sid >> showError "glAttachShader") sdrs
			glLinkProgram pid
			alloca $ \pint -> do
				glGetProgramiv pid (marshal LinkStatus) pint
				linkStatus <- peek pint
				if linkStatus == 0 then do
					glGetProgramiv pid (marshal ProgramInfoLogLength) pint
					len <- peek pint
					msg <- allocaBytes (fromIntegral len) $ \buf -> do
						glGetProgramInfoLog pid len nullPtr buf
						msg <- peekCStringLen (buf,fromIntegral len)
						return $ "Could not link program " ++ name ++ "\n" ++ msg
					putStrLn msg
					glDeleteProgram pid
					return (Left [msg])
				else return . Right $ arg pid sdrs


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

blobContent (Blob b) = b

loadShader :: Shader -> IO (Either String ShaderId)
loadShader s =
	case s of
		VertexShader name blob -> go 0x8B31 name blob
		FragmentShader name blob -> go 0x8B30 name blob
	where go shaderType name blob = do
		sid <- glCreateShader shaderType
		if sid == 0 then do
			showError "glCreateShader"
			return . Left $ "glCreateShader returned 0."
		else do
			B.useAsCString (blobContent blob) $ \src -> do
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
								glGetShaderInfoLog sid len nullPtr buf
								msg <- peekCStringLen (buf,fromIntegral len)
								return $ "Could not compile " ++ " " ++ name ++ "\n" ++ msg
							putStrLn msg
							glDeleteShader sid
							return (Left msg)
						else return $ Right sid

getAttribLocation :: ProgramId -> String -> IO AttrId
getAttribLocation prog name = do
	withCString name (liftA fromIntegral . glGetAttribLocation prog)

setVertexAttr :: VertexAttr -> IO ()
setVertexAttr (VertexAttr name typ srcty normalize stride offset array loc) = do
	putStrLn name
	--bindArray array
	--glVertexAttribPointer loc size (marshal srcty) (fromBool normalize) stride offset
	glEnableVertexAttribArray loc
setVertexAttr (ConstantVA name value loc) = do
	glDisableVertexAttribArray loc
	case value of
		Attr1f x -> glVertexAttrib1f loc x
		Attr2f x y -> glVertexAttrib2f loc x y
		Attr3f x y z -> glVertexAttrib3f loc x y z
		Attr4f x y z w -> glVertexAttrib4f loc x y z w
		Attr4i x y z w -> glVertexAttribI4i loc x y z w
		Attr4ui x y z w -> glVertexAttribI4ui loc x y z w
		-- XXX try Vec and glVertexAttribNfv

getUniformLocation :: ProgramId -> String -> IO UniformId
getUniformLocation prog name =
	withCString name (glGetUniformLocation prog)

setUniform :: UniformVar -> IO ()
setUniform (UniformVar _ val loc) =
	case val of
		Uniform1f x -> glUniform1f loc x
		Uniform2f x y -> glUniform2f loc x y
		Uniform3f x y z -> glUniform3f loc x y z
		Uniform4f x y z w -> glUniform4f loc x y z w
		Uniform1i x -> glUniform1i loc x
		Uniform2i x y -> glUniform2i loc x y
		Uniform3i x y z -> glUniform3i loc x y z
		Uniform4i x y z w -> glUniform4i loc x y z w
		UniformMatrix2 a b c d ->
			withArray [a,b,c,d] (glUniformMatrix2fv loc 1 0)
		-- ...
		Uniform1ui x -> glUniform1ui loc x
		Uniform2ui x y -> glUniform2ui loc x y
		Uniform3ui x y z -> glUniform3ui loc x y z
		Uniform4ui x y z w -> glUniform4ui loc x y z w

data Capability =
	  Texture2D
	| CullFace
	| Blend
	| Dither
	| StencilTest
	| DepthTest
	| ScissorTest
	| PolygonOffsetFill
	| SampleAlphaToCoverage
	| SampleCoverage
	| PrimitiveRestartFixedIndex -- ^ ES 3.0

instance Marshal Capability where
	marshal x = case x of
		Texture2D             -> 0x0DE1
		CullFace              -> 0x0B44
		Blend                 -> 0x0BE2
		Dither                -> 0x0BD0
		StencilTest           -> 0x0B90
		DepthTest             -> 0x0B71
		ScissorTest           -> 0x0C11
		PolygonOffsetFill     -> 0x8037
		SampleAlphaToCoverage -> 0x809E
		SampleCoverage        -> 0x80A0
		PrimitiveRestartFixedIndex -> 0x8D69

enable :: Capability -> IO ()
enable = glEnable . marshal

disable :: Capability -> IO ()
disable = glDisable . marshal

isEnabled :: Capability -> IO Bool
isEnabled = liftA (/= 0) . glIsEnabled . marshal
