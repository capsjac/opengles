module Graphics.OpenGLES.Core where
import qualified Data.ByteString as B
import Control.Applicative
import Foreign.C.String
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.OpenGLES.Base

-- * Core Data Types

data DrawCall = DrawCall
	{ dcMode :: DrawMode
	, dcProgram :: Program
	, dcCount :: Indices
	, dcAttrs :: [VertexAttr]
	, dcUniforms :: [UniformVar]
	, dcTexture :: [Texture]
	, dcConfig :: DrawConfig
	}
	deriving (Show, Eq)
-- XXX: DrawTexture Extension

data Blob = Blob { blobContent :: B.ByteString }
	| BufferId GLuint
	deriving (Show, Eq)


data DrawMode = Points | Lines | LineLoop | LineStrip
              | Triangles | TriangleStrip | TriangleFan
              deriving (Show, Eq)


-- ** Vertex Indices

type Indices = Int


-- ** Shader

data Program = Program String [Shader]
	| ProgramId GLuint [GLuint]
	deriving (Show, Eq)

{-data Shader = Shader
	{ s_type :: ShaderType
	, s_source :: Blob
	, s_attribs :: [String]
	, s_uniforms :: [String]
	}
	deriving (Show, Eq)-}
data Shader = VertexShader
		{ sdrName :: String
		, sdrSource :: Blob
		}
	| FragmentShader
		{ sdrName :: String
		, sdrSource :: Blob
		}
	| ShaderId GLuint
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
		}
	| ConstantVA
		{ attrVarName :: String
		, attrValue :: AttrValue
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

data UniformVar =
	  UniformVar
		{ uniformName :: String
		, uniformValue :: UniformValue
		}
	| UniformId GLint
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
	, texLevel :: Int
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

drawData :: DrawCall -> IO (Either [String] DrawCall)
drawData d@(DrawCall mode prog count attrs vars tex conf) = do
	putStrLn "draw start"
	compiled <- loadProgram prog
	case compiled of
		err@(Left _) -> return err
		Right prog -> do

			vars <- mapM (getUniformLocation prog . uniformName) attrs

			return $ Right d { dcProgram = prog, dcUniforms = vars }
		
	-- vertex attribute

	-- uniform variable
	
	let setCapability bool = if bool then enable else disable
	setCapability (blendEnable conf) Blend
	setCapability (cullFaceEnable conf) CullFace
	setCapability (depthTextEnable conf) DepthTest
	glDepthMask (fromBool $ depthMaskEnable conf)

	glDrawArrays (marshal mode) 0 (fromIntegral count)


-- call glPixelStorei GL_[UN]PACK_ALIGNMENT [1248] before texImage2d

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

loadProgram :: Program -> IO (Either [String] Program)
loadProgram (Program name shaders) = do
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
			mapM (\(ShaderId sid)->
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
				else return . Right $ ProgramId	pid (map (\(ShaderId i)->i) sdrs)


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

loadShader :: Shader -> IO (Either String Shader)
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
						else return . Right . ShaderId $ sid
--getAttribLocation :: ProgramId -> String -> IO AttrLoc
--getAttribLocation (ProgramId prog) name = do
	--withCString name $ \str -> do
		--AttrLoc . fromIntegral <$> glGetAttribLocation prog str

getUniformLocation :: (Num a) => Program -> String -> IO UniformVar
getUniformLocation (ProgramId prog) name = do
	withCString name $ \str -> do
		UniformId <$> glGetUniformLocation prog str

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
