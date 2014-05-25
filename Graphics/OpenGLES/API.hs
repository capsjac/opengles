{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Graphics.OpenGLES.API where
import Control.Applicative
import Data.Bits ((.|.))
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.OpenGLES.Base
import Unsafe.Coerce

-- * Utils
-- | fromEnum alternative
class Marshal a where marshal :: (Num n) => a -> n

-- ** Errors
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

-- ** Buffer Objects
-- | EnableCap
data OpenGLCapability =
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

instance Marshal OpenGLCapability where
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

enable :: OpenGLCapability -> IO ()
enable = glEnable . marshal

disable :: OpenGLCapability -> IO ()
disable = glDisable . marshal

isEnabled :: OpenGLCapability -> IO Bool
isEnabled = liftA (/= 0) . glIsEnabled . marshal

-- | glClear
clearBuffer :: Bool -- ^ Clear color buffer
            -> Bool -- ^ Clear depth buffer
            -> Bool -- ^ Clear stencil buffer
            -> IO ()
clearBuffer c d s = glClear $ (if d then 0x100 else 0)
  .|. (if s then 0x400 else 0) .|. (if c then 0x4000 else 0)

getGLVendor = glGetString 0x1F00 >>= peekCString
getGLRenderer = glGetString 0x1F01 >>= peekCString
getGLVersion = glGetString 0x1F02 >>= peekCString
getGLExtensions = words <$> (glGetString 0x1F03 >>= peekCString)
getGLShadingLanguageVersion = glGetString 0x8B8C >>= peekCString

data CullFaceMode = Front | Back | FrontAndBack

instance Marshal CullFaceMode where
	marshal Front        = 0x0404
	marshal Back         = 0x0405
	marshal FrontAndBack = 0x0408

stencilMaskSeparate :: CullFaceMode -> GLuint -> IO ()
stencilMaskSeparate face mask = glStencilMaskSeparate (marshal face) mask

data HintTarget = GenerateMipmapHint
                | FragmentShaderDerivativeHint -- ^ ES 3.0

instance Marshal HintTarget where
	marshal GenerateMipmapHint = 0x8192
	marshal FragmentShaderDerivativeHint = 0x8B8B

data HintMode = DontCare | Fastest | Nicest

instance Marshal HintMode where
	marshal DontCare = 0x1100
	marshal Fastest  = 0x1101
	marshal Nicest   = 0x1102

hint :: HintTarget -> HintMode -> IO ()
hint target hintmode = glHint (marshal target) (marshal hintmode) 

data StencilFunction =
	  SFNever | SFLess | SFEqual | SFLEqual | SFGreater
	| SFNotEqual | SFGEqual | SFAlways

instance Marshal StencilFunction where
	marshal x = case x of
		SFNever    -> 0x0200
		SFLess     -> 0x0201
		SFEqual    -> 0x0202
		SFLEqual   -> 0x0203
		SFGreater  -> 0x0204
		SFNotEqual -> 0x0205
		SFGEqual   -> 0x0206
		SFAlways   -> 0x0207

depthFunc :: StencilFunction -> IO ()
depthFunc = glDepthFunc . marshal

data StencilOp =
	  OpZero | OpKeep | OpReplace | OpIncr
	| OpDecr | OpInvert | OpIncrWrap | OpDecrWrap

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

stencilOp :: StencilOp -> StencilOp -> StencilOp -> IO ()
stencilOp sfail dpfail dppass =
	glStencilOp (marshal sfail) (marshal dpfail) (marshal dppass)

stencilOpSeparate :: CullFaceMode -> StencilOp -> StencilOp -> StencilOp -> IO ()
stencilOpSeparate face sfail dpfail dppass =
	glStencilOpSeparate (marshal face) (marshal sfail)
	                    (marshal dpfail) (marshal dppass)

data FrontFaceDirection = CW | CCW
frontFace CW = glFrontFace 0x0900
frontFace CCW = glFrontFace 0x0901

cullFace :: CullFaceMode -> IO ()
cullFace = glCullFace . marshal

-- | An abstraction layer for glGen*, glBind*, glIs* and glDelete*
class ServerObject a where
	genObjects :: Int -> IO [a]
	bindObject :: (BindTarget a b) => b -> a -> IO ()
	isObject :: a -> IO Bool
	deleteObjects :: [a] -> IO ()

class (Marshal b) => BindTarget a b

newtype BufferId = BufferId { unBufferId :: GLuint }

instance ServerObject BufferId where
	genObjects n = allocaArray n $ \arr -> do
		glGenBuffers (fromIntegral n) arr
		map BufferId <$> peekArray n arr
	bindObject target (BufferId x) = glBindBuffer (marshal target) x
	isObject (BufferId x) = return . (== 1) =<< glIsBuffer x
	deleteObjects xs = withArray (map unBufferId xs) $ \arr ->
		glDeleteBuffers (fromIntegral $ length xs) arr

data BufferTarget = ArrayBuffer
                  | ElementArrayBuffer
                  | PixelPackBuffer -- ^ ES 3.0
                  | PixelUnpackBuffer -- ^ ES 3.0
                  | CopyReadBuffer -- ^ ES 3.0
                  | CopyWriteBuffer -- ^ ES 3.0
-- | ES 3.0
data BufferTarget' = TransformFeedbackBuffer | UniformBuffer

instance BindTarget	BufferId BufferTarget
instance BindTarget	BufferId BufferTarget'
instance Marshal BufferTarget where
	marshal x = case x of
		ArrayBuffer -> 0x8892
		ElementArrayBuffer -> 0x8893
		PixelPackBuffer -> 0x88EB
		PixelUnpackBuffer -> 0x88EC
		CopyReadBuffer -> 0x8F36
		CopyWriteBuffer -> 0x8F37
instance Marshal BufferTarget' where
	marshal TransformFeedbackBuffer = 0x8C8E
	marshal UniformBuffer = 0x8A11


data FramebufferId = FramebufferId { unFramebufferId :: GLuint }

instance ServerObject FramebufferId where
	genObjects n = allocaArray n $ \arr -> do
		glGenFramebuffers (fromIntegral n) arr
		map FramebufferId <$> peekArray n arr
	bindObject target (FramebufferId x) =
		glBindFramebuffer (marshal target) x
	isObject (FramebufferId x) =
		return . (== 1) =<< glIsFramebuffer x
	deleteObjects xs = withArray (map unFramebufferId xs) $ \arr ->
		glDeleteFramebuffers (fromIntegral $ length xs) arr

data FramebufferTarget = Framebuffer
data FramebufferTarget' = Framebuffer'
                        | DrawFramebuffer -- ^ ES 3.0
                        | ReadFramebuffer -- ^ ES 3.0
instance BindTarget	FramebufferId FramebufferTarget
instance Marshal FramebufferTarget where
	marshal Framebuffer = 0x8D40


data RenderbufferId = RenderbufferId { unRenderbufferId :: GLuint }

instance ServerObject RenderbufferId where
	genObjects n = allocaArray n $ \arr -> do
		glGenRenderbuffers (fromIntegral n) arr
		map RenderbufferId <$> peekArray n arr
	bindObject target (RenderbufferId x) =
		glBindRenderbuffer (marshal target) x
	isObject (RenderbufferId x) =
		return . (== 1) =<< glIsRenderbuffer x
	deleteObjects xs = withArray (map unRenderbufferId xs) $ \arr ->
		glDeleteRenderbuffers (fromIntegral $ length xs) arr

data RenderbufferTarget = Renderbuffer
instance BindTarget	RenderbufferId RenderbufferTarget
instance Marshal RenderbufferTarget where
	marshal Renderbuffer = 0x8D40


data TextureId = TextureId { unTextureId :: GLuint }

instance ServerObject TextureId where
	genObjects n = allocaArray n $ \arr -> do
		glGenTextures (fromIntegral n) arr
		map TextureId <$> peekArray n arr
	bindObject target (TextureId x) =
		glBindTexture (marshal target) x
	isObject (TextureId x) =
		return . (== 1) =<< glIsTexture x
	deleteObjects xs = withArray (map unTextureId xs) $ \arr ->
		glDeleteTextures (fromIntegral $ length xs) arr

{-data TextureTarget =
	  Tex2D
	| TexCubeMap
	| TexCubeMapPosX | TexCubeMapPosY | TexCubeMapPosZ
	| TexCubeMapNegX | TexCubeMapNegY | TexCubeMapNegZ
	| Tex3D -- ^ ES 3.0
	| Tex2DArray -- ^ ES 3.0

instance BindTarget	TextureId TextureTarget
instance Marshal TextureTarget where
	marshal x = case x of
		Tex2D          -> 0x0DE1
		TexCubeMap     -> 0x8513
		TexCubeMapPosX -> 0x8515
		TexCubeMapPosY -> 0x8517
		TexCubeMapPosZ -> 0x8519
		TexCubeMapNegX -> 0x8516
		TexCubeMapNegY -> 0x8518
		TexCubeMapNegZ -> 0x851A
		Tex3D          -> 0x806F
		Tex2DArray     -> 0x8C1A
-}

-- | ES 3.0
data QueryId = QueryId { unQueryId :: GLuint }

instance ServerObject QueryId where
	genObjects n = allocaArray n $ \arr -> do
		glGenQueries (fromIntegral n) arr
		map QueryId <$> peekArray n arr
	bindObject _ _ = fail "bindObject for a Query does not exist."
	isObject (QueryId x) = return . (== 1) =<< glIsQuery x
	deleteObjects xs = withArray (map unQueryId xs) $ \arr ->
		glDeleteQueries (fromIntegral $ length xs) arr

-- | ES 3.0
data QueryTarget = AnySamplesPassed | AnySamplesPassedConservative
instance BindTarget	QueryId QueryTarget
instance Marshal QueryTarget where
	marshal AnySamplesPassed = 0x8C2F
	marshal AnySamplesPassedConservative = 0x8D6A


-- | ES 3.0
data TransformFeedback = TransformFeedbackId { unTransformFeedback :: GLuint }

instance ServerObject TransformFeedback where
	genObjects n = allocaArray n $ \arr -> do
		glGenTransformFeedbacks (fromIntegral n) arr
		map TransformFeedbackId <$> peekArray n arr
	bindObject target (TransformFeedbackId x) =
		glBindTransformFeedback (marshal target) x
	isObject (TransformFeedbackId x) =
		return . (== 1) =<< glIsTransformFeedback x
	deleteObjects xs = withArray (map unTransformFeedback xs) $ \arr ->
		glDeleteTransformFeedbacks (fromIntegral $ length xs) arr

-- | ES 3.0
data TransformFeedbackTarget = TransformFeedback
instance BindTarget	TransformFeedback TransformFeedbackTarget
instance Marshal TransformFeedbackTarget where
	marshal TransformFeedback = 0x8E22

{-
teximg2d's data::Maybe Ptr
bindzero to clear
Buffer: {ELEMENT_}ARRAY_BUFFER

TexImage2D,CopyTexImage2D,CopyTexSubImage2D,CompressedTexImage2D,CompressedTexSubImage2D
	b TEXTURE_2D, TEXTURE_CUBE_MAP_POSITIVE_{X,Y,Z},TEXTURE_CUBE_MAP_NEGATIVE_{X,Y,Z} 
TexSubImage2D,
	b* TEXTURE_CUBE_MAP_POSITIVE_{X, Y, Z},TEXTURE_CUBE_MAP_NEGATIVE_{X, Y, Z}
TexParameter{if}v?,GenerateMipmap,GetTexParameter{if}v:
	a TEXTURE_2D, TEXTURE_CUBE_MAP
BindTexture; a+b

BindFramebuffer,FramebufferTexture2D,CheckFramebufferStatus,GetFramebufferAttachmentParameteriv:
	FRAMEBUFFER

BindRenderbuffer,RenderbufferStorage,GetRenderbufferParameteriv:
	RENDERBUFFER

FramebufferRenderbuffer
-}
{-
Query: ANY_SAMPLES_PASSED{_CONSERVATIVE}
BindBuffer and rest:
	{ELEMENT_}ARRAY_BUFFER, PIXEL_{UN}PACK_BUFFER,
	COPY_{READ, WRITE}_BUFFER,
	TRANSFORM_FEEDBACK_BUFFER, UNIFORM_BUFFER
BindBufferRange
	TRANSFORM_FEEDBACK_BUFFER, UNIFORM_BUFFER
BindBufferBase
	TRANSFORM_FEEDBACK_BUFFER, UNIFORM_BUFFER

TransformFeedback: TRANSFORM_FEEDBACK
Framebuffer: FRAMEBUFFER
FramebufferRenderbuffer,FramebufferTexture2D,CheckFramebufferStatus,GetFramebufferAttachmentParameteriv:
	FRAMEBUFFER,{DRAW,READ}_FRAMEBUFFER
Renderbuffer: RENDERBUFFER
Texture: a+b+c
TexImage3D,TexStorage3D,TexSubImage3D,CopyTexSubImage3D,CompressedTexImage3D,: 
	c TEXTURE_3D, TEXTURE_2D_ARRAY
TexStorage2D:
	a TEXTURE_CUBE_MAP, TEXTURE_2D
TexImage2D,CopyTexImage2D,TexSubImage2D,CopyTexSubImage2D,CompressedTexImage2D,CompressedTexSubImage2D,CompressedTexSubImage3D:
	b TEXTURE_2D, TEXTURE_CUBE_MAP_POSITIVE_{X, Y, Z},TEXTURE_CUBE_MAP_NEGATIVE_{X, Y, Z}
TexParameter{i,f}{,v},GenerateMipmap:
	a+c TEXTURE_{2D, 3D}, TEXTURE_2D_ARRAY, TEXTURE_CUBE_MAP

3.0 TEXTURE_3D, TEXTURE_2D_ARRAY
-}

newtype ShaderId = ShaderId { unShaderId :: GLuint } deriving Eq

data ShaderType = FragmentShader_ | VertexShader_ deriving (Show, Eq)
instance Marshal ShaderType where
	marshal FragmentShader_ = 0x8B30
	marshal VertexShader_   = 0x8B31

data ShaderPName = ShaderType
                 | CompileStatus
                 | ShaderInfoLogLength
                 | ShaderSourceLength
                 | ShaderCompiler
                 -- and more on ES 3.0
instance Marshal ShaderPName where
	marshal x = case x of
		ShaderType -> 0x8B4F
		CompileStatus -> 0x8B81
		ShaderInfoLogLength -> 0x8B84
		ShaderSourceLength -> 0x8B88
		ShaderCompiler -> 0x8DFA
		-- ...

loadShader :: ShaderType -> String -> String -> IO (Either String ShaderId)
loadShader shaderType name code = do
	shader <- glCreateShader (marshal shaderType)
	if shader /= 0 then
		withCString code $ \src -> do
			withArray [src] $ \ptr -> do
				glShaderSource shader 1 ptr nullPtr
				glCompileShader shader
				alloca $ \pint -> do
					glGetShaderiv shader (marshal CompileStatus) pint
					compiled <- peek pint
					if compiled == 0 then do
						glGetShaderiv shader (marshal ShaderInfoLogLength) pint
						len <- peek pint
						msg <- allocaBytes (fromIntegral len) $ \buf -> do
							glGetShaderInfoLog shader len nullPtr buf
							msg <- peekCStringLen (buf,fromIntegral len)
							return $ "Could not compile " ++ show shaderType
								++ " " ++ name ++ "\n" ++ msg
						putStrLn msg
						glDeleteShader shader
						return (Left msg)
					else return . Right . ShaderId $ shader
	else do
		showError "glCreateShader"
		return . Left $ "glCreateShader returned 0."

newtype ProgramId = ProgramId { unProgramId :: GLuint }

data ProgramPName = DeleteStatus
                  | LinkStatus
                  | ValidateStatus
                  | ProgramInfoLogLength
                  | AttachedShaders
                  | ActiveAttributes
                  | ActiveAttributeMaxLength
                  | ActiveUniforms
                  | ActiveUniformMaxLength
                  -- and more and more on ES 3.0
instance Marshal ProgramPName where
	marshal x = case x of
		DeleteStatus -> 0x8B80
		LinkStatus -> 0x8B82
		ValidateStatus -> 0x8B83
		AttachedShaders -> 0x8B85
		ActiveAttributes -> 0x8B89
		ActiveUniformMaxLength -> 0x8B8A
		-- ......

createProgram :: [(String,String)]
              -> [(String,String)]
              -> IO (Either [String] ProgramId)
createProgram vertexCodes fragmentCodes = do
	vx <- mapM (\(n,c)->loadShader VertexShader_ n c) vertexCodes
	fx <- mapM (\(n,c)->loadShader FragmentShader_ n c) fragmentCodes
	let right (Right _) = True; right _ = False
	let lefts = filter (not.right) (vx ++ fx)
	let lft (Left x) = x; rht (Right x) = x
	if lefts /= [] then return . Left $ map lft lefts
	else do
		let sdrs = map rht (vx ++ fx)
		program <- glCreateProgram
		if program /= 0 then do
			let attach shader = do
				glAttachShader program (unShaderId shader)
				showError "glAttachShader"
			mapM attach sdrs
			glLinkProgram program
			alloca $ \pint -> do
				glGetProgramiv program (marshal LinkStatus) pint
				linkStatus <- peek pint
				if linkStatus == 0 then do
					glGetProgramiv program (marshal ProgramInfoLogLength) pint
					len <- peek pint
					msg <- allocaBytes (fromIntegral len) $ \buf -> do
						glGetProgramInfoLog program len nullPtr buf
						msg <- peekCStringLen (buf,fromIntegral len)
						return $ "Could not link program:\n" ++ msg
					putStrLn msg
					glDeleteProgram program
					return (Left [msg])
				else return . Right . ProgramId $ program
		else do
			showError "glCreateProgram"
			return (Left ["glCreateProgram returned 0."])

showError :: String -> IO ()
showError location = do
	getError >>= \err -> case err of
		NoError -> return ()
		_ -> putStrLn $ "GLError " ++ location ++ ": " ++ show err

newtype AttrLoc = AttrLoc { unAttrLoc :: GLuint }

getAttribLocation :: ProgramId -> String -> IO AttrLoc
getAttribLocation (ProgramId prog) name = do
	withCString name $ \str -> do
		AttrLoc . fromIntegral <$> glGetAttribLocation prog str

newtype UnifLoc = UnifLoc { unUnifLoc :: GLint }

getUniformLocation :: (Num a) => ProgramId -> String -> IO UnifLoc
getUniformLocation (ProgramId prog) name = do
	withCString name $ \str -> do
		UnifLoc <$> glGetUniformLocation prog str

useProgram :: ProgramId -> IO ()
useProgram (ProgramId p) = glUseProgram p

deleteProgram :: ProgramId -> IO ()
deleteProgram (ProgramId p) = glDeleteProgram p

data DataType =
	  ByteT
	| UByteT
	| ShortT
	| UShortT
	| IntT  -- ^ Cannot used in vertexAttribPointer
	| UIntT -- ^ Cannot used in vertexAttribPointer
	| FloatT
	| FixedT
	-- and more on ES 3.0
instance Marshal DataType where
	marshal x = case x of
		ByteT -> 0x1400
		UByteT -> 0x1401
		ShortT -> 0x1402
		UShortT -> 0x1403
		IntT -> 0x1404
		UIntT -> 0x1405
		FloatT -> 0x1406
		FixedT -> 0x140C
		-- ...

vertexAttribPointer :: AttrLoc -> Int -> DataType -> Bool -> Int -> Ptr GLfloat -> IO ()
vertexAttribPointer (AttrLoc index) size typ normalized stride ptr =
	glVertexAttribPointer index (fromIntegral size)
	                      (marshal typ) (fromBool normalized)
	                      (fromIntegral stride) (unsafeCoerce ptr)

vertexAttribPointerArrayBufBound :: AttrLoc -> Int -> DataType -> Bool -> Int -> Int -> IO ()
vertexAttribPointerArrayBufBound (AttrLoc index) size typ normalized stride offset =
	glVertexAttribPointer index (fromIntegral size)
	                      (marshal typ) (fromBool normalized)
	                      (fromIntegral stride) (unsafeCoerce offset)

enableVertexAttribArray :: AttrLoc -> IO ()
enableVertexAttribArray (AttrLoc index) =
	glEnableVertexAttribArray index

data DrawMode = Points | Lines | LineLoop | LineStrip
              | Triangles | TriangleStrip | TriangleFan
              deriving (Show, Eq)
instance Marshal DrawMode where
	marshal x = case x of
		Points -> 0
		Lines -> 1
		LineLoop -> 2
		LineStrip -> 3
		Triangles -> 4
		TriangleStrip -> 5
		TriangleFan -> 6

drawArrays :: DrawMode -> Int -> Int -> IO ()
drawArrays mode first count =
	glDrawArrays (marshal mode) (fromIntegral first) (fromIntegral count)

detectGLESVersion :: Int
detectGLESVersion =
	let es2 = isGLProcAvailable "glCreateShader" in
	let es3 = isGLProcAvailable "glIsQuery" in
	case (es2,es3) of
		(True, True)  -> 3
		(True, False) -> 2
		_             -> 1

class Uniform a where
	-- | set a value to an uniform variable
	uniform :: UnifLoc -> a -> IO ()
	uniform (UnifLoc loc) = uniform_ loc
	
	uniform_ :: GLint -> a -> IO ()
	
	setUniform :: ProgramId -> String -> a -> IO ()
	setUniform p name a =
		getUniformLocation p name >>= \loc-> uniform loc a

instance Uniform GLfloat where
	uniform_ = glUniform1f
instance Uniform (GLfloat,GLfloat) where
	uniform_ loc (x,y) = glUniform2f loc x y
instance Uniform (GLfloat,GLfloat,GLfloat) where
	uniform_ loc (x,y,z) = glUniform3f loc x y z
instance Uniform (GLfloat,GLfloat,GLfloat,GLfloat) where
	uniform_ loc (x,y,z,a) = glUniform4f loc x y z a
instance Uniform GLint where
	uniform_ = glUniform1i
instance Uniform (GLint,GLint) where
	uniform_ loc (x,y) = glUniform2i loc x y
instance Uniform (GLint,GLint,GLint) where
	uniform_ loc (x,y,z) = glUniform3i loc x y z
instance Uniform (GLint,GLint,GLint,GLint) where
	uniform_ loc (x,y,z,a) = glUniform4i loc x y z a

-- | Uniform (GLuint..) since ES 3.0
instance Uniform GLuint where
	uniform_ = glUniform1ui
instance Uniform (GLuint,GLuint) where
	uniform_ loc (x,y) = glUniform2ui loc x y
instance Uniform (GLuint,GLuint,GLuint) where
	uniform_ loc (x,y,z) = glUniform3ui loc x y z
instance Uniform (GLuint,GLuint,GLuint,GLuint) where
	uniform_ loc (x,y,z,a) = glUniform4ui loc x y z a

--class UniformVec a where
--	uniformv :: GLuint -> GLsizei -> Ptr a -> IO ()

data BufferUsage =
	  StreamDraw
	| StaticDraw
	| DynamicDraw
	| StreamRead -- ^ ES 3.0
	| StreamCopy -- ^ ES 3.0
	| StaticRead -- ^ ES 3.0
	| StaticCopy -- ^ ES 3.0
	| DynamicRead -- ^ ES 3.0
	| DynamicCopy -- ^ ES 3.0
instance Marshal BufferUsage where
	marshal x = case x of
		StreamDraw -> 0x88E0
		StreamRead -> 0x88E1
		StreamCopy -> 0x88E2
		StaticDraw -> 0x88E4
		StaticRead -> 0x88E5
		StaticCopy -> 0x88E6
		DynamicDraw -> 0x88E8
		DynamicRead -> 0x88E9
		DynamicCopy -> 0x88EA


bufferData :: BufferTarget -> Int -> Ptr a -> BufferUsage -> IO ()
bufferData target size ptr usage = do
	glBufferData (marshal target) (fromIntegral size)
	             (castPtr ptr) (marshal usage)

uniformMatrixf2 :: UnifLoc -> Int -> Bool -> [GLfloat] -> IO ()
uniformMatrixf2 (UnifLoc loc) count transpose value =
	withArray value $ \ptr ->
		glUniformMatrix2fv loc (fromIntegral count) (fromBool transpose)
	                       ptr

uniformMatrixf3 :: UnifLoc -> Int -> Bool -> [GLfloat] -> IO ()
uniformMatrixf3 (UnifLoc loc) count transpose value =
	withArray value $ \ptr ->
		glUniformMatrix2fv loc (fromIntegral count) (fromBool transpose)
	                       ptr

uniformMatrixf4 :: UnifLoc -> Int -> Bool -> [GLfloat] -> IO ()
uniformMatrixf4 (UnifLoc loc) count transpose value =
	withArray value $ \ptr ->
		glUniformMatrix2fv loc (fromIntegral count) (fromBool transpose)
	                       ptr

viewport :: (Integral a, Integral b) => a -> a -> b -> b -> IO ()
viewport x y w h = glViewport (fromIntegral x) (fromIntegral y)
                              (fromIntegral w) (fromIntegral h)

flashCommands :: IO ()
flashCommands = glFlush

waitForFinish :: IO ()
waitForFinish = glFinish

data SyncResult = AlreadySignaled
                | ConditionSatisfied
                | TimeoutExpired
                | WaitFailed 
                | InvalidValue_

-- | better glFinish for ES 3.0
waitForGPUCommandsComplete :: (Integral a)
                           => Bool -- ^ flag for GL_SYNC_FLUSH_COMMANDS_BIT
                           -> a -- ^ timeout in nanosecond
                           -> IO SyncResult
waitForGPUCommandsComplete flashCmds timeout_ns = do
	sync <- glFenceSync 0x9117 0 -- GL_SYNC_GPU_COMMANDS_COMPLETE
	res <- glClientWaitSync sync (fromBool flashCmds) $ fromIntegral timeout_ns
	glDeleteSync sync
	return $ case res of
		0x911A -> AlreadySignaled -- GL_ALREADY_SIGNALED
		0x911C -> ConditionSatisfied -- GL_CONDITION_SATISFIED
		0x911B -> TimeoutExpired -- GL_TIMEOUT_EXPIRED
		0x911D -> WaitFailed -- GL_WAIT_FAILED
		0x0501 -> InvalidValue_ -- GL_INVALID_VALUE

-- | ES 3.0
blockGPUWhileDraw :: IO ()
blockGPUWhileDraw = do
	sync <- glFenceSync 0x9117 0 -- GL_SYNC_GPU_COMMANDS_COMPLETE
	glWaitSync sync 0 0xFFFFFFFFFFFFFFFF -- GL_TIMEOUT_IGNORED
	glDeleteSync sync

