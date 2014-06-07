{-# LANGUAGE FlexibleInstances, BangPatterns, StandaloneDeriving #-}
-- | GL ES 3.1 is not supported yet.
module Graphics.OpenGLES.Core where
import qualified Data.ByteString as B
import Control.Applicative
import Data.Either
import Data.Vect
import Foreign
import Foreign.C.String
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import Graphics.OpenGLES.Base
import System.IO.Unsafe (unsafePerformIO)


-- * Core Data Types

data DrawCall =
	  DrawUnit -- ^ You should use this constructor.
		DrawMode
		(ProgramRef -> [ShaderRef] -> Program)
		DrawConfig
		[UniformId -> UniformVar]
		[AttrId -> VertexAttr]
		[TextureRef -> Texture]
		Int -- ^ First vertex index
		Int -- ^ The number of vertices to draw
	| DrawArrays -- ^ Internally used
		!DrawMode !Program !DrawConfig
		![UniformVar] ![VertexAttr] ![Texture] !Int !Int
	| DrawArraysInstanced -- ^ Internally used
		!DrawMode !Program !DrawConfig
		![UniformVar] ![VertexAttr] ![Texture] !Int !Int
	| DrawElements -- ^ Internally used
		!DrawMode !Program !DrawConfig
		![UniformVar] ![VertexAttr] ![Texture] --!Count !Indices !IndexType(u8,u16) !ByteOffset
	| DrawTexture !Vec2 !Vec2 -- XXX DrawImage
	deriving (Show, Eq)

instance Show (ProgramRef -> [ShaderRef] -> Program) where show x = show (x nullForeignPtr [])
instance Show (AttrId -> VertexAttr) where show x = show (x 0)
instance Show (UniformId -> UniformVar) where show x = show (x 0)
instance Show (TextureRef -> Texture) where show x = show (x nullForeignPtr)
instance Eq (ProgramRef -> [ShaderRef] -> Program) where x == y = x nullForeignPtr [] == y nullForeignPtr []
instance Eq (AttrId -> VertexAttr) where x == y = x 0 == y 0
instance Eq (UniformId -> UniformVar) where x == y = x 0 == y 0
instance Eq (TextureRef -> Texture) where x == y = x nullForeignPtr == y nullForeignPtr

type BufferRef = ForeignPtr GLuint
type ProgramRef = ForeignPtr GLuint
type ShaderRef = ForeignPtr GLuint
type AttrId = GLuint
type UniformId = GLint
type TextureRef = ForeignPtr GLuint

nullForeignPtr = unsafePerformIO $ newForeignPtr_ nullPtr

data Blob = Blob !B.ByteString
	deriving (Show, Eq)

-- ** Draw mode

data DrawMode = Points | Lines | LineLoop | LineStrip
              | Triangles | TriangleStrip | TriangleFan
              deriving (Show, Eq)


-- ** Shader

data Program =
	Program
		String  -- ^ A program name. This name will be shown in error messages and used as binary cache key.
		[Shader] -- ^ Shaders. One or more 'VertexShader' and 'FragmentShader' can be specified. Note: Only 1 main() is allowed for each type of shaders.
		!ProgramRef
		[ShaderRef]
	deriving (Show, Eq)
	-- GLManager type: add setAPIVersion, setCompiledProgs[(name,blob)], getAllProgramBinaries

-- | Shader encoding is UTF-8 (ES 3.0+) or ASCII (2.0).
data Shader =
	  VertexShader String B.ByteString
	| FragmentShader String B.ByteString
	deriving (Show, Eq)


-- ** Graphic State

data DrawConfig = DrawConfig
	{ blendEnable :: !Bool
	, cullFaceEnable :: !Bool
	, depthMaskEnable :: !Bool
	, depthTextEnable :: !Bool
	}
	deriving (Show, Eq)


-- ** Vertex Attribute

data VertexAttr =
	  Vertex String VertexData AttrId
	-- ^ for vec[1234]
	| NormalizedVertex String VertexData AttrId
	-- ^ Given values are clamped to [0,1] ([-1,1] if signed type.)
	| IntVertex String VertexData AttrId
	-- ^ for ivec[1234]
	| Instanced !Int !(AttrId -> VertexAttr) !AttrId
	-- ^ Treat as: let vertexList' = concat $ map (replicate instanceNum) vertexList
	| BufferSlice String !BufferRef !GLint !GLenum !GLboolean !GLsizei !Int !AttrId
	-- ^ Internally used. Wrapping glVertexAttribPointer()
	| BufferSlicei String !BufferRef !GLint !GLenum !GLboolean !GLsizei !Int !AttrId
	-- ^ Internally used. Wrapping glVertexAttribIPointer()
	| ConstAttr1f String !Float !AttrId
	| ConstAttr2f String !Vec2 !AttrId
	| ConstAttr3f String !Vec3 !AttrId
	| ConstAttr4f String !Vec4 !AttrId
	| ConstAttr4i String !GLint !GLint !GLint !GLint !AttrId -- ^ introduced in ES 3.0
	| ConstAttr4ui String !GLuint !GLuint !GLuint !GLuint !AttrId -- ^ introduced in ES 3.0
	deriving (Show, Eq)

attrVarName :: VertexAttr -> String
attrVarName x = case x of
	Vertex s _ _ -> s; NormalizedVertex s _ _ -> s;
	IntVertex s _ _ -> s; Instanced _ s _ -> attrVarName (s 0);
	BufferSlice s _ _ _ _ _ _ _ -> s;
	BufferSlicei s _ _ _ _ _ _ _ -> s;
	ConstAttr1f s _ _ -> s; ConstAttr2f s _ _ -> s;
	ConstAttr3f s _ _ -> s; ConstAttr4f s _ _ -> s;
	ConstAttr4i s _ _ _ _ _ -> s; ConstAttr4ui s _ _ _ _ _ -> s;

-- | Vector array
data VertexData =
	  ByteV Int [Int8]
	| UByteV Int [Word8]
	| ShortV Int [Int16]
	| UShortV Int [Word16]
	| IntV Int [Int32]  -- ^ introduced in ES 3.0
	| UIntV Int [Word32]  -- ^ introduced in ES 3.0
	-- | HalfFloatV Int [Word16?]
	| FloatV Int [Float]
	-- | FixedV Int [Word32?]
	-- | I2_10_10_10V [Word32]
	-- | UI2_10_10_10V [Word32]
	-- | BlobSliced
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
	| Uniform1i !GLint
	| Uniform2i !(GLint, GLint) -- should make sth like IVec2?
	| Uniform3i !(GLint, GLint, GLint)
	| Uniform4i !(GLint, GLint, GLint, GLint)
	| Uniform1iv ![GLint]
	| Uniform2iv ![(GLint, GLint)]
	| Uniform3iv ![(GLint, GLint, GLint)]
	| Uniform4iv ![(GLint, GLint, GLint, GLint)]
	| UniformMat2 !Mat2
	| UniformMat3 !Mat3
	| UniformMat4 !Mat4
	| UniformMat2v ![Mat2]
	| UniformMat3v ![Mat3]
	| UniformMat4v ![Mat4]
	| UniformSampler !GLint -- XXX
	| UniformSamplers ![GLint]
	-- ES 3.0
	| Uniform1ui !GLuint
	| Uniform2ui !(GLuint, GLuint)
	| Uniform3ui !(GLuint, GLuint, GLuint)
	| Uniform4ui !(GLuint, GLuint, GLuint, GLuint)
	| Uniform1uiv ![GLuint]
	| Uniform2uiv ![(GLuint, GLuint)]
	| Uniform3uiv ![(GLuint, GLuint, GLuint)]
	| Uniform4uiv ![(GLuint, GLuint, GLuint, GLuint)]
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
	, texRef :: TextureRef
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
		{ magFilter :: !MagFilter
		, minFilter :: !MinFilter
		, wrapS :: !WrapMode
		, wrapT :: !WrapMode
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


-- ** Vertex Indices

type Indices = Int



-- * Data-driven Rendering

-- ** Initialization

data GLManager = GLManager
	{ glAPIVersion :: GLVersion -- ^ set API version to be used
	, compiledProgramCache :: [(String, B.ByteString)]
	-- ^ get/set after/before program linkage
	--, glRefHolder :: [ForeignPtr GLuint]
	}

data GLVersion = ES2 | ES3 | ES31 deriving Show


-- ** Draw
-- | Allocate video memory for rendering in advance and compile a call
-- that optimised for the running platform.
{-compileCall :: GLManager -- ^ API version and program binary caches
            -> DrawCall -- ^ DrawCall to compile
            -> IO (Either [String] DrawCall) -- ^ Return errors or compiled call
compileCall
		(GLManager ver cache)
		d@(DrawUnit mode prog conf uniforms attribs texes from to) = do

					pid' <- bindFinalizer glDeleteProgram pid
					sdrs' <- mapM (bindFinalizer glDeleteShader) sdrs
					return . Right $ arg pid' sdrs'
	compiled <- loadProgram prog
	case compiled of
		Left err -> return $ Left err
		Right prog -> do
			pid <- withForeignPtr (progRef prog) (return.ptrToId)
			attr' <- mapM (\a -> do
				let name = attrVarName $ a 0
				i <- getAttribLocation pid name
				if i < 0 then error $ "Vertex attribute '" ++ name
					++ "' was not found in shader program '" ++ progName prog
					++ "'"
				else return $ a i
				) attr
			uni' <- mapM (\u -> do
				let name = uniformName $ u 0
				i <- getUniformLocation pid name
				if i < 0 then error $ "Uniform variable '" ++ name
					++ "' was not found in shader program '" ++ progName prog
					++ "'"
				else return $ u i
				) uni
			-- array buffer
			let tex' = map ($ 0) tex
			return . Right $ DrawArrays	mode prog count attr' uni' tex' conf
-}

drawData :: DrawCall -> IO ()
drawData (DrawArrays mode prog conf uniforms attribs texes from to) = do
	-- shader setting
	let Program _ _ progRef _ = prog
	withForeignPtr progRef (glUseProgram . ptrToId)
	
	-- vertex attribute
	mapM_ setVertexAttr attribs
	
	-- uniform variable
	mapM_ setUniformVar uniforms
	
	-- texture
	
	-- draw config
	let setCapability bool = if bool then enable else disable
	setCapability (blendEnable conf) Blend
	setCapability (cullFaceEnable conf) CullFace
	setCapability (depthTextEnable conf) DepthTest
	glDepthMask (fromBool $ depthMaskEnable conf)

	glDrawArrays (marshal mode) (fromIntegral from) (fromIntegral to)
-- DrawArraysInstanced
-- call glPixelStorei GL_[UN]PACK_ALIGNMENT [1248] before texImage2d


-- * Internals

withEither :: IO (Either a b) -> (b -> IO (Either a b)) -> IO (Either a b)
withEither result cont = do
	m <- result
	case m of
		err@(Left _) -> return err
		Right x -> cont x

type ResourceId = GLuint

-- ** Garbage collection for GPU objects
foreign import ccall "wrapper"
	wrapFinalizerPtr :: (Ptr a -> IO ()) -> IO (FinalizerPtr a)

bindFinalizer :: (ResourceId -> IO ()) -> ResourceId -> IO (ForeignPtr ResourceId)
bindFinalizer finalizer i = do
	f <- wrapFinalizerPtr (\ptr -> finalizer (ptrToId ptr))
	newForeignPtr f (idToPtr i)

idToPtr i = nullPtr `plusPtr` fromIntegral i
ptrToId ptr = fromIntegral $ ptr `minusPtr` nullPtr
finalize = finalizeForeignPtr



-- ** Wrappers
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

loadProgram :: String -> [Shader] -> IO (Either [String] [ResourceId])
loadProgram progname shaders = do
	results <- mapM loadShader shaders
	let (errors, resids) = partitionEithers results
	if errors /= [] then return $ Left errors
	else do
		pid <- glCreateProgram
		if pid == 0 then do
			showError "glCreateProgram"
			return (Left ["glCreateProgram returned 0."])
		else do
			mapM_ (\sid ->
				glAttachShader pid sid >> showError "glAttachShader") resids
			glLinkProgram pid
			alloca $ \pint -> do
				glGetProgramiv pid (marshal LinkStatus) pint
				linkStatus <- peek pint
				if linkStatus == 0 then do
					glGetProgramiv pid (marshal ProgramInfoLogLength) pint
					len <- peek pint
					msg <- allocaBytes (fromIntegral len) $ \buf -> do
						glGetProgramInfoLog pid len nullPtr buf
						msg <- peekCStringLen (buf, fromIntegral len)
						return $ "Cannot link program " ++ progname ++ "\n" ++ msg
					putStrLn msg
					glDeleteProgram pid
					return (Left [msg])
				else return $ Right (pid : resids)

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
								glGetShaderInfoLog sid len nullPtr buf
								msg <- peekCStringLen (buf, fromIntegral len)
								return $ "Could not compile " ++ " " ++ name ++ "\n" ++ msg
							putStrLn msg
							glDeleteShader sid
							return (Left msg)
						else return $ Right sid

getAttribLocation :: ResourceId -> String -> IO AttrId
getAttribLocation prog name =
	withCString name (liftA fromIntegral . glGetAttribLocation prog)

getUniformLocation :: ResourceId -> String -> IO UniformId
getUniformLocation prog name =
	withCString name (glGetUniformLocation prog)


setVertexAttr :: VertexAttr -> IO ()
setVertexAttr va = case va of
	Vertex _ va loc -> error "setVertexAttr: VertexAttr must be compiled!"
	NormalizedVertex _ va loc -> error "setVertexAttr: VertexAttr must be compiled!"
	IntVertex _ va loc -> error "setVertexAttr: VertexAttr must be compiled!"
	{-case va of
		ByteV n xs -> 
		UByteV n xs -> 
		ShortV n xs -> 
		UShortV n xs -> 
		IntV n xs -> 
		UIntV n xs -> 
		--HalfFloatV n xs -> 
		FloatV n xs -> 
		-- FixedV n xs ->
		-- I2_10_10_10V xs ->
		-- UI2_10_10_10V xs ->
		-- BlobSliced ->
		NoneV -> return ()-}
	Instanced divNum f loc -> do
		setVertexAttr (f loc)
		glVertexAttribDivisor loc (fromIntegral divNum)
	BufferSlice _ ref size typ normalize stride offset loc -> do
		withForeignPtr ref (glBindBuffer . ptrToId)
		glVertexAttribPointer loc size typ normalize stride (idToPtr offset)
		glEnableVertexAttribArray loc
	BufferSlicei _ ref size typ normalize stride offset loc -> do
		withForeignPtr ref (glBindBuffer . ptrToId)
		glVertexAttribIPointer loc size typ normalize stride (idToPtr offset)
		glEnableVertexAttribArray loc
	ConstAttr1f _ x loc -> d loc >> glVertexAttrib1f loc (r x)
	ConstAttr2f _ (Vec2 x y) loc -> d loc >> glVertexAttrib2f loc (r x) (r y)
	ConstAttr3f _ (Vec3 x y z) l -> d l >> glVertexAttrib3f l (r x) (r y) (r z)
	ConstAttr4f _ (Vec4 x y z w) l -> d l >> glVertexAttrib4f l (r x) (r y) (r z) (r w)
	ConstAttr4i _ x y z w loc -> d loc >> glVertexAttribI4i loc x y z w
	ConstAttr4ui _ x y z w loc -> d loc >> glVertexAttribI4ui loc x y z w
	where
		r = realToFrac
		d = glDisableVertexAttribArray


setUniformVar :: UniformVar -> IO ()
setUniformVar (UniformVar _ val loc) =
	case val of
		Uniform1f x -> glUniform1f loc $ r x
		Uniform2f (Vec2 x y) -> glUniform2f loc (r x) (r y)
		Uniform3f (Vec3 x y z) -> glUniform3f loc (r x) (r y) (r z)
		Uniform4f (Vec4 x y z w) -> glUniform4f loc (r x) (r y) (r z) (r w)
		Uniform1fv xs -> withArray xs (glUniform1fv loc (len xs))
		Uniform2fv xs -> withArray xs (glUniform2fv loc (len xs))
		Uniform3fv xs -> withArray xs (glUniform3fv loc (len xs))
		Uniform4fv xs -> withArray xs (glUniform4fv loc (len xs))
		Uniform1i x -> glUniform1i loc x
		Uniform2i (x,y) -> glUniform2i loc x y
		Uniform3i (x,y,z) -> glUniform3i loc x y z
		Uniform4i (x,y,z,w) -> glUniform4i loc x y z w
		Uniform1iv xs -> withArray xs (glUniform1iv loc (len xs))
		Uniform2iv xs -> withArray xs (glUniform2iv loc (len xs))
		Uniform3iv xs -> withArray xs (glUniform3iv loc (len xs))
		Uniform4iv xs -> withArray xs (glUniform4iv loc (len xs))
		UniformMat2 m -> with m (glUniformMatrix2fv loc 1 1)
		UniformMat3 m -> with m (glUniformMatrix3fv loc 1 1)
		UniformMat4 m -> with m (glUniformMatrix4fv loc 1 1)
		UniformMat2v mx -> withArray mx (glUniformMatrix2fv loc (len mx) 1)
		UniformMat3v mx -> withArray mx (glUniformMatrix3fv loc (len mx) 1)
		UniformMat4v mx -> withArray mx (glUniformMatrix4fv loc (len mx) 1)
		UniformSampler x -> glUniform1i loc x
		UniformSamplers xs -> withArray xs (glUniform1iv loc (len xs))
		Uniform1ui x -> glUniform1ui loc x
		Uniform2ui (x,y) -> glUniform2ui loc x y
		Uniform3ui (x,y,z) -> glUniform3ui loc x y z
		Uniform4ui (x,y,z,w) -> glUniform4ui loc x y z w
		Uniform1uiv xs -> withArray xs (glUniform1uiv loc (len xs))
		Uniform2uiv xs -> withArray xs (glUniform2uiv loc (len xs))
		Uniform3uiv xs -> withArray xs (glUniform3uiv loc (len xs))
		Uniform4uiv xs -> withArray xs (glUniform4uiv loc (len xs))
	where
		r = realToFrac :: Float -> GLfloat
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


-- ** Vector Utils

structMat2 :: [Float] -> Mat2
structMat2 [a,b,c,d] = Mat2 (Vec2 a b) (Vec2 c d)
structMat2 xs = error $ "structMat2: not a 2x2 matrix: " ++ show xs

structMat3 :: [Float] -> Mat3
structMat3 [a,b,c,d,e,f,g,h,i] = Mat3 (Vec3 a b c) (Vec3 d e f)(Vec3 g h i)
structMat3 xs = error $ "structMat3: not a 3x3 matrix: " ++ show xs

structMat4 :: [Float] -> Mat4
structMat4 [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p] =
	Mat4 (Vec4 a b c d) (Vec4 e f g h) (Vec4 i j k l) (Vec4 m n o p)
structMat4 xs = error $ "structMat4: not a 4x4 matrix: " ++ show xs

