{-# LANGUAGE FlexibleInstances, BangPatterns, StandaloneDeriving #-}
-- | GL ES 3.1 is not supported yet.
module Graphics.OpenGLES.Core where
import qualified Data.ByteString as B
import Data.ByteString.Internal (create, nullForeignPtr, toForeignPtr)
import Control.Applicative
import Control.Monad
import Data.Either
import Data.IORef
import Data.Vect
import Foreign hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.C.String
import Foreign.Concurrent -- (GHC only)
import Graphics.OpenGLES.Base


-- * Core Data Types

data DrawCall =
	  DrawUnit -- ^ You should use this constructor.
		DrawMode
		(ProgramRef -> [ShaderRef] -> Program)
		DrawConfig
		[UniformId -> UniformVar]
		[AttrId -> VertexAttr]
		[TextureRef -> Texture]
		VertexPicker
	| DrawCall
		!DrawMode !Program !DrawConfig
		![UniformVar] ![VertexAttr] ![Texture] !VertexPicker
	| DrawTexture !TextureRef !Int !Int !Int !Int !Int !Int !Int !Int !Int -- add DrawConfig?
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
	| BufferSlicei String !BufferRef !GLint !GLenum !GLsizei !Int !AttrId
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
	BufferSlicei s _ _ _ _ _ _ -> s;
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
	, texColorFormat :: TextureColorFormat
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
	-- | _3Dc -- ATI, NVidia
	-- | Palette

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


-- ** Vertex Picker

-- | VFromCounts -> VFromCounts', VIndex8/16/32 -> VIndex(Instanced)',
-- VIndices8/16/32 -> VIndices' or DrawCallSequence[VIndex(Instanced)']
--
-- Version/Ext fallback feature is not yet. (See above)
data VertexPicker =
	  VFromCount !Int !Int
	| VFromCountInstanced !Int !Int !Int
	| VFromCounts ![(Int, Int)]
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
-- that optimised for the running platform.
compileCall :: GLManager -- ^ API version and program binary caches
            -> DrawCall -- ^ DrawCall to compile
            -> IO (Either [String] DrawCall) -- ^ Return errors or compiled call
compileCall glm@(GLManager version cache)
	(DrawUnit mode prog conf uniforms attribs texes picker) = do

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
			let name = attrVarName $ attr 0
			i <- getAttribLocation pid name
			showError $ "glGetAttribLocation " ++ show (progName, name, i)
			if i < 0 then error $ "Vertex attribute '" ++ name
				++ "' was not found in shader program '" ++ progName
				++ "'"
			else return $ attr $ fromIntegral i
		
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
		
		let textures = map ($ nullFPtr) texes
		
		picker' <- compilePicker False picker
		return . Right $ DrawCall mode prog conf unifs attrs textures picker'

	return d

eitherIO :: (Monad m) => Either a b -> (b -> m (Either a c)) -> m (Either a c)
eitherIO (Right r) f = f r
eitherIO (Left l) _ = return (Left l)

drawData :: DrawCall -> IO ()
drawData (DrawCall mode prog conf uniforms attribs texes picker) = do
	-- shader setting
	let Program _ _ progRef _ = prog
	withForeignPtr progRef (glUseProgram . ptrToId)
	
	-- vertex attribute
	mapM_ setVertexAttr attribs
	
	-- uniform variable
	mapM_ setUniformVar uniforms
	
	-- texture
	-- call glPixelStorei GL_[UN]PACK_ALIGNMENT [1248] before texImage2d

	-- draw config
	let setCapability bool = if bool then enable else disable
	setCapability (blendEnable conf) Blend
	setCapability (cullFaceEnable conf) CullFace
	setCapability (depthTextEnable conf) DepthTest
	glDepthMask (fromBool $ depthMaskEnable conf)

	invokeDraw mode picker

drawData (DrawTexture ref u v tw th x y z w h) = do
	-- GL_TEXTURE_2D = 0x0DE1, GL_TEXTURE_CROP_RECT_OES = 0x8B9D
	withForeignPtr ref (glBindTexture 0x0DE1 . ptrToId)
	withArray [r u,r v,r tw,r th] (glTexParameteriv 0x0DE1 0x8B9D)
	glDrawTexiOES (r x) (r y) (r z) (r w) (r h) -- disable AlphaTest?
	where r = fromIntegral



-- * Internals

-- ** Garbage collection for GPU objects

bindFinalizer :: IO () -> ResourceId -> IO (ForeignPtr ResourceId)
bindFinalizer f i = newForeignPtr (idToPtr i) (putStrLn "Fin" >> f)

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
						glGetProgramInfoLog pid len nullPtr buf
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
								glGetShaderInfoLog sid len nullPtr buf
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

compileVertexAttr :: VertexAttr -> IO VertexAttr
compileVertexAttr va = case va of
	Vertex name vect loc -> compileVertex name vect loc 0
	NormalizedVertex name vect loc -> compileVertex name vect loc 1
	IntVertex name vect loc ->
		return va -- XXX go name vect loc BufferSlicei
	Instanced divNum f loc -> do
		--compiled <- compileVertexAttr (f loc)
		--Instanced divNum compiled loc
		-- XXX fallback
		return va
	--BufferSlice _ ref size typ normalize stride offset loc -> do	
	--BufferSlicei _ ref size typ stride offset loc -> do
	-- ConstAttr[1234]f/4i/4ui
	_ -> return va

gl_array_buffer = 0x8892
gl_static_draw = 0x88E4

compileVertex name vect loc normalize = case vect of
	--ByteV n xs -> 
	UByteV n xs -> do
		bidptr <- malloc
		bid <- glGenBuffers 1 bidptr >> peek bidptr
		showError "glGenBuffers"
		putStrLn $ show bid
		glBindBuffer gl_array_buffer bid
		showError "glBindBuffer"
		elems <- withArrayLen xs $ \len ptr -> do
			glBufferData gl_array_buffer (int' $ len * 1) (castPtr ptr) gl_static_draw
			showError "glBufferData"
			return len
		fpb <- bindFinalizer (glDeleteBuffers 1 bidptr >> free bidptr) bid
		-- GL_UNSIGNED_UBYTE = 0x1401
		return $ BufferSlice name fpb (int $ elems `div` n) 0x1401 normalize (int n) 0 loc
	--ShortV n xs -> 
	--UShortV n xs -> 
	--IntV n xs -> 
	--UIntV n xs -> 
	--HalfFloatV n xs -> -}
	FloatV n xs -> do
		-- XXX error if elems%n /= 0
		bidptr <- malloc
		bid <- glGenBuffers 1 bidptr >> peek bidptr
		putStrLn $ show bid
		showError "glGenBuffers"
		glBindBuffer gl_array_buffer bid
		elems <- withArrayLen xs $ \len ptr -> do
			glBufferData gl_array_buffer (int' $ len * 4) (castPtr ptr) gl_static_draw
			showError "glBufferData"
			return len
		fpb <- bindFinalizer (glDeleteBuffers 1 bidptr >> free bidptr) bid
		return $ BufferSlice name fpb (int $ elems `div` n) 0x1406 normalize (4 * int n) 0 loc
	-- FixedV n xs ->
	-- I2_10_10_10V xs ->
	-- UI2_10_10_10V xs ->
	-- BlobSliced ->
	NoneV -> return $ Vertex name NoneV loc
	where int = fromIntegral; int' = fromIntegral

compilePicker :: Bool -> VertexPicker -> IO VertexPicker
compilePicker instanced vp = case vp of
	VFromCount first count -> return vp
	VFromCountInstanced first count divNum -> return vp
	VFromCounts list -> do
		let (firsts, counts) = unzip list
		let (first', count') = (map size firsts, map size counts)
		firstbs <- create (length list * 4) (flip pokeArray first' . castPtr)
		countbs <- create (length list * 4) (flip pokeArray count' . castPtr)
		return $ VFromCounts' firstbs countbs
	VFromCounts' firsts counts -> return vp
	VIndex8 index -> return vp -- XXX
	VIndex16 index -> return vp -- XXX
	VIndex32 index -> return vp -- XXX
	VIndex' bref count typ offset -> return vp
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
	where size = fromIntegral :: (Integral a) => a -> GLsizei

setVertexAttr :: VertexAttr -> IO ()
setVertexAttr va = case va of
	Vertex _ va loc -> error "setVertexAttr: VertexAttr must be compiled!"
	NormalizedVertex _ va loc -> error "setVertexAttr: VertexAttr must be compiled!"
	IntVertex _ va loc -> error "setVertexAttr: VertexAttr must be compiled!"
	Instanced divNum f loc -> do
		setVertexAttr (f loc)
		glVertexAttribDivisor loc (fromIntegral divNum)
	BufferSlice _ ref size typ normalize stride offset loc -> do
		withForeignPtr ref (glBindBuffer 0x8892 . ptrToId)
		glVertexAttribPointer loc size typ normalize stride (idToPtr offset)
		showError "glVertexAttribPointer"
		glEnableVertexAttribArray loc
		showError "glEnableVertexAttribArray"
	BufferSlicei _ ref size typ stride offset loc -> do
		withForeignPtr ref (glBindBuffer 0x8892 . ptrToId)
		glVertexAttribIPointer loc size typ stride (idToPtr offset)
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
		UniformSampler x -> glUniform1i loc x
		UniformSamplers xs -> withArray xs (glUniform1iv loc (len xs))
		Uniform1ui x -> glUniform1ui loc x
		Uniform2ui (x,y) -> glUniform2ui loc x y
		Uniform3ui (x,y,z) -> glUniform3ui loc x y z
		Uniform4ui (x,y,z,w) -> glUniform4ui loc x y z w
		{-Uniform1uiv xs -> withArray xs (glUniform1uiv loc (len xs))
		Uniform2uiv xs -> withArray xs (glUniform2uiv loc (len xs))
		Uniform3uiv xs -> withArray xs (glUniform3uiv loc (len xs))
		Uniform4uiv xs -> withArray xs (glUniform4uiv loc (len xs))-}
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
	| PrimitiveRestartFixedIndex -- ^ introduced in ES 3.0
	| RasterizerDiscard -- ^ introduced in ES 3.0
	-- | SampleMask -- ^ introduced in ES 3.1

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
		RasterizerDiscard     -> 0x8C89
		-- SampleMask -> 0x

enable :: Capability -> IO ()
enable = glEnable . marshal

disable :: Capability -> IO ()
disable = glDisable . marshal

isEnabled :: Capability -> IO Bool
isEnabled = liftA (/= 0) . glIsEnabled . marshal

withBSBS :: B.ByteString -> B.ByteString
         -> (Ptr a -> Ptr b -> Int -> IO c)
         -> IO c
withBSBS bs1 bs2 f =
	withForeignPtr fp1 $ \p1 ->
		withForeignPtr fp2 $ \p2 ->
			f (castPtr $ p1 `plusPtr` off1) (castPtr $ p2 `plusPtr` off2) len
	where
		(fp1,off1,len) = toForeignPtr bs1
		(fp2,off2,_) = toForeignPtr bs2

invokeDraw :: DrawMode -> VertexPicker -> IO ()
invokeDraw mode picker = case picker of
	VFromCount first count -> do
		glDrawArrays m (int first) (int count)
		showError "glDrawArrays"
	VFromCountInstanced first count primcount ->
		glDrawArraysInstanced m (int first) (int count) (int primcount)
	VFromCounts list ->
		forM_ list (\(fst, cnt) -> glDrawArrays m (int fst) (int cnt))
	VFromCounts' firstbs countbs ->
		withBSBS countbs firstbs $ \cptr fptr clen ->
			glMultiDrawArraysEXT m fptr cptr
				(int $ clen `div` sizeOf (0 :: GLsizei))
	VIndex' ref count typ offset -> do
		-- bind GL_ELEMENT_ARRAY_BUFFER = 0x8893
		withForeignPtr ref (glBindBuffer 0x8893 . ptrToId)
		glDrawElements m count typ (idToPtr offset)
	VIndexInstanced' ref count typ offset divNum ->
		glDrawElementsInstanced m count typ (idToPtr offset) divNum
	--VIndices8/16/32 ... ->
	VIndices' countbs typ indicesbs ->
		withBSBS countbs indicesbs $ \cptr iptr clen ->
			glMultiDrawElementsEXT m cptr typ iptr
				(int $ clen `div` sizeOf (0 :: GLsizei))
	-- VFromToIndex' ...
	DrawCallSequence xs -> mapM_ (invokeDraw mode) xs
	_ -> error $ "invokeDraw: VertexPicker (" ++ show picker ++ ") must be compiled."
	where
		m = marshal mode
		int = fromIntegral
