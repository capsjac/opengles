{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Graphics.OpenGLES.Internal where
import Control.Applicative
import Control.Monad
import Control.Concurrent.Chan
import Control.Future
import Data.Array.Storable (StorableArray)
import qualified Data.ByteString as B
import Data.IORef
import Data.Typeable
import Foreign hiding (newForeignPtr, addForeignPtrFinalizer, void)
import Foreign.C.String (peekCString, peekCStringLen)
import Foreign.Concurrent (newForeignPtr, addForeignPtrFinalizer)
import Graphics.OpenGLES.Base
import Linear.Vect
import System.IO.Unsafe (unsafePerformIO)

-- * Internal

-- glRestoreLostObjects :: GL ()
-- saveBuffer :: Buffer -> IO ()
-- saveBuffer buf = atomicModifyIORef' (buf:) bufferArchive
-- bufferArchive = unsafePerformIO $ newIORef []
-- addCompiledProgramResources

frameCounter :: IORef Int
frameCounter = unsafePerformIO $ newIORef 0

-- ** Logging

errorQueue :: Chan String
errorQueue = unsafePerformIO newChan
{-# NOINLINE errorQueue #-}

glLog :: String -> IO ()
glLog msg = writeChan errorQueue msg


-- ** GL Error

data GLError = InvalidEnum | InvalidValue | InvalidOperation
             | OutOfMemory | InvalidFrameBufferOperation
             deriving Show

getError :: GL (Maybe GLError)
getError = unMarshal <$> glGetError
	where unMarshal x = case x of
		0x0000 -> Nothing
		0x0500 -> Just InvalidEnum
		0x0501 -> Just InvalidValue
		0x0502 -> Just InvalidOperation
		0x0505 -> Just OutOfMemory
		0x0506 -> Just InvalidFrameBufferOperation

showError :: String -> GL Bool
showError location = do
	putStrLn location -- tmp
	getError >>= maybe (return False) (\err -> do
		glLog ("E " ++ location ++ ": " ++ show err)
		return True )

-- ** GL Object management
type GLO = IORef GLObj
data GLObj = GLObj GLuint (GL GLObj) (ForeignPtr GLuint)

getObjId glo = fmap go (readIORef glo)
	where go (GLObj i _ _) = i

instance Show GLO where
	show = show . unsafePerformIO . getObjId

newGLO
	:: (GLsizei -> Ptr GLuint -> GL ())
	-> (GLsizei -> Ptr GLuint -> GL ())
	-> (GLuint -> GL ())
	-> GL GLO
newGLO gen del init = do
	ref <- newIORef undefined
	writeIORef ref =<< genObj gen del init
	-- addToGLOMS ref
	return ref

-- | genObj glo glGenBuffers glDeleteBuffers
genObj
	:: (GLsizei -> Ptr GLuint -> GL ())
	-> (GLsizei -> Ptr GLuint -> GL ())
	-> (GLuint -> GL ())
	-> GL GLObj
genObj genObjs delObjs initObj = do
	fp <- mallocForeignPtr
	withForeignPtr fp $ \ptr -> do
		genObjs 1 ptr
		showError "genObj"
		obj <- peek ptr
		addForeignPtrFinalizer fp $ do
			-- XXX check whether context is valud or not
			with obj $ \ptr -> do
				delObjs 1 ptr
				void $ showError "delObj"
		initObj obj
		return $ GLObj obj (genObj genObjs delObjs initObj) fp


-- ** Types
-- VertexArray
-- 2.0
newtype HalfFloat = HalfFloat Word16 deriving (Num,Storable)
newtype FixedFloat = FixedFloat Int32 deriving (Num,Storable)
-- 3.0
newtype Int2_10x3 = Int210x3 Int32 deriving (Num,Storable)
newtype Word2_10x3 = Word2_10x3 Int32 deriving (Num,Storable)

-- Renderbuffer
-- 2.0
newtype Word4444 = Word4444 Word16 deriving (Num,Storable)
newtype Word5551 = Word5551 Word16 deriving (Num,Storable)
newtype Word565 = Word565 Word16 deriving (Num,Storable)
-- 3.0
newtype Word10f11f11f = Word10f11f11f Word32 deriving (Num,Storable)
newtype Word5999 = Word5999 Word32 deriving (Num,Storable)
newtype Word24_8 = Word24_8 Word32 deriving (Num,Storable)
newtype FloatWord24_8 = FloatWord24_8 (Float, Word32)

class GLType a where
	glType :: m a -> Word32

instance GLType Int8 where glType _ = 0x1400
instance GLType Word8 where glType _ = 0x1401
instance GLType Int16 where glType _ = 0x1402
instance GLType Word16 where glType _ = 0x1403
instance GLType Int32 where glType _ = 0x1404
instance GLType Word32 where glType _ = 0x1405

instance GLType Float where glType _ = 0x1406
instance GLType HalfFloat where glType _ = 0x140B
instance GLType FixedFloat where glType _ = 0x140C
instance GLType Int2_10x3 where glType _ = 0x8D9F
instance GLType Word2_10x3 where glType _ = 0x8368

instance GLType Word4444 where glType _ = 0x8033
instance GLType Word5551 where glType _ = 0x8034
instance GLType Word565 where glType _ = 0x8363
instance GLType Word10f11f11f where glType _ = 0x8C3B
instance GLType Word5999 where glType _ = 0x8C3E
instance GLType Word24_8 where glType _ = 0x84FA
instance GLType FloatWord24_8 where glType _ = 0x8DAD

r,rg,rgb,rgba,r_integer,rg_integer,rgb_integer,rgba_integer,
	depth_component,depth_stencil :: GLenum
rgb = 0x1907
rgba = 0x1908
depth_component = 0x1902

r = 0x1903
rg = 0x8227
rg_integer = 0x8228
r_integer = 0x8D94
rgb_integer = 0x8D98
rgba_integer = 0x8D99
depth_stencil = 0x84F9


-- ** Buffer

-- Buffer usage id (latestArray or length)
data Buffer a = Buffer 	(IORef (Either (StorableArray Int a) Int)) GLO
-- DoubleBuffer GLO GLO (IORef (GLArray a))

newtype BufferUsage = BufferUsage GLenum

newtype BufferSlot = BufferSlot GLenum

-- ** DrawMode

newtype DrawMode = DrawMode GLenum


-- ** Graphics State

newtype Capability = Capability GLenum
newtype CullFace = Culling GLenum
newtype CompFunc = CompFunc GLenum
newtype StencilOp = StencilOp GLenum
newtype BlendOp = BlendOp GLenum
newtype BlendingFactor = BlendingFactor GLenum
newtype Hint = Hint GLenum


-- ** Programmable Shader

type ShaderType = GLenum

data Shader = Shader ShaderType GLName B.ByteString
	deriving Show

data TransformFeedback =
	  NoFeedback
	| FeedbackArrays [String]
	| FeedbackPacked [String]
	deriving Show

data Program p = Program
	{ programGLO :: GLO
	, programTF :: TransformFeedback
	, programShaders :: [Shader]
	, programVariables :: ([VarDesc], [VarDesc])
	} deriving Show

type ProgramBinary = B.ByteString

-- | name: (location, length of array, type)
type VarDesc = (String, (GLint, GLsizei, GLenum))

-- binaryStore :: IORef [(String, B.ByteString)]
-- or (FilePath -> IO B.ByteString)
-- binaryStore = unsafePerformIO $ newIORef []

programDict :: IORef [(String, Program ())]
programDict = unsafePerformIO $ newIORef []

lookupVarDesc :: TypeRep -> IO (Maybe ([VarDesc], [VarDesc]))
lookupVarDesc rep = do
	let name = show rep
	entry <- lookup name <$> readIORef programDict
	case entry of
		Nothing -> do
			glLog $ "Program '" ++ name ++ "' is not compiled."
			return Nothing
		Just prog -> return $ Just (programVariables prog)

loadProgram
	:: Typeable p
	=> Program p
	-> (Int -> String -> Maybe ProgramBinary -> GL ())
	-> GL (Progress [String] (Program p))
loadProgram prog@(Program glo tf shaders ([],[])) progressLogger = do
	let numShaders = length shaders
	let progname = show (typeRep prog)
	let msg = "Start compiling: " ++ progname
	glLog msg
	progressLogger 0 msg Nothing
	
	pid <- glCreateProgram
	res <- if pid == 0 then do
		showError "glCreateProgram"
		let msg = "Fatal: glCreateProgram returned 0."
		progressLogger (numShaders + 1) msg Nothing
		return $ Fixme [msg]
	else do
		results <- mapM (loadShader progressLogger) (zip [1..] shaders)
		-- putStrLn $ show results
		let errors = [msg | Fixme [msg] <- results]
		res <- if errors /= []
		then return $ Fixme errors
		else do
			forM_ results $ \(Finished sid) -> do
				glAttachShader pid sid
				showError "glAttachShader"
			glLinkProgram pid
			showError "glLinkProgram"
			postLink progname numShaders prog pid progressLogger
		sequence_ [glDeleteShader s | Finished s <- results]
		return res
	glLog "---------------"
	return res

postLink
	:: Typeable p
	=> String -> Int -> Program p -> GLuint
	-> (Int -> String -> Maybe ProgramBinary -> GL ())
	-> GL (Progress [String] (Program p))
postLink progname numShaders prog pid
		progressLogger = alloca $ \intptr -> do
	glGetProgramiv pid c_link_status intptr
	linkStatus <- peek intptr
	glGetProgramiv pid c_info_log_length intptr
	len <- fmap fromIntegral $ peek intptr
	info <- allocaBytes len $ \buf -> do
		glGetProgramInfoLog pid (fromIntegral len) nullPtr buf
		peekCStringLen (buf, len-1)
	let info' = if info == "" then "" else '\n':info
	if linkStatus == 0 then do
		let msg = "Cannot link program " ++ progname ++ info'
		glLog msg
		progressLogger (numShaders + 1) msg Nothing
		glDeleteProgram pid
		return $ Fixme [msg]
	else do
		-- obtain shader variables
		vars <- getActiveVariables pid
		putStrLn . show $ vars
		fp <- newForeignPtr nullPtr (glDeleteProgram pid)
		writeIORef (programGLO prog) (GLObj pid (error "not impl: Program implicit recompilation") fp)
		let msg = "Successfully linked " ++ progname ++ "!" ++ info'
		glLog msg
		progressLogger (numShaders + 1) msg Nothing
		let prog' = prog { programVariables = vars }
		atomicModifyIORef' programDict $! \xs ->
			((show (typeRep prog), prog'):xs, ())
		return $ Finished prog'

c_link_status = 0x8B82
c_info_log_length = 0x8B84

{-
GL_PROGRAM_BINARY_RETRIEVABLE_HINT               0x8257
GL_PROGRAM_BINARY_LENGTH                         0x8741
GL_NUM_PROGRAM_BINARY_FORMATS                    0x87FE
loadProgramBinary :: Program p -> GLuint -> GL ()
loadProgramBinary (Program tf _ ref) pid = do
	bs <- ...
	let (fp, offset, len) = toForeignPtr bs
	withForeignPtr fp $ \p -> do
		fmt <- peek (p `plusPtr` offset)
		glProgramBinary pid fmt (p `plusPtr` (offset+4)) (fromIntegral len)
		showError "glProgramBinary"
		if err, writeIORef ref Broken
	postLink progname numShaders ref pid
-}

loadShader
	:: (Int -> String -> Maybe ProgramBinary -> GL ())
	-> (Int, Shader)
	-> GL (Progress [String] GLuint)
loadShader progressLogger (i, Shader shaderType name bs) = do
	sid <- glCreateShader shaderType
	if sid == 0 then do
		showError "glCreateShader"
		let msg = "Fatal: glCreateShader returned 0."
		glLog msg
		progressLogger i msg Nothing
		return $ Fixme [name ++ ": " ++ msg]
	else B.useAsCString bs $ \src -> do
		withArray [src] $ \ptr -> do
			glShaderSource sid 1 ptr nullPtr
			showError "glShaderSource"
			glCompileShader sid
			showError "glCompileShader"
			alloca $ \pint -> do
				glGetShaderiv sid c_compile_status pint
				compiled <- peek pint
				glGetShaderiv sid c_info_log_length pint
				len <- fmap fromIntegral $ peek pint
				info <- allocaBytes len $ \buf -> do
					glGetShaderInfoLog sid (fromIntegral len) nullPtr buf
					peekCStringLen (buf, len-1)
				let info' = if info == "" then "" else '\n':info
				if compiled == 0 then do
					let msg = "Could not compile " ++ name ++ info'
					glLog msg
					progressLogger i msg Nothing
					glDeleteShader sid
					return $ Fixme [msg]
				else do
					let msg = name ++ " ... done" ++ info'
					glLog msg
					progressLogger i msg Nothing
					return $ Finished sid

c_compile_status = 0x8B81

getActiveVariables :: GLuint -> GL ([VarDesc], [VarDesc])
getActiveVariables pid = do
	sptr <- malloc
	glGetProgramiv pid c_active_uniform_max_length sptr
	uMaxLen <- peek sptr
	glGetProgramiv pid c_active_attribute_max_length sptr
	aMaxLen <- peek sptr
	let maxlen = max uMaxLen aMaxLen
	str <- mallocBytes (fromIntegral maxlen)
	
	glGetProgramiv pid c_active_uniforms sptr
	numU <- peek sptr
	glGetProgramiv pid c_active_attributes sptr
	numA <- peek sptr
	
	tptr <- malloc
	uniforms <- forM [0..numU-1] $ \ index -> do
		-- avoid [0..maxBound] bug
		let i = (fromIntegral :: GLint -> GLuint) index
		glGetActiveUniform pid i maxlen nullPtr sptr tptr str
		name <- peekCString str
		loc <- glGetUniformLocation pid str
		size <- peek sptr
		typ <- peek tptr
		return (name, (loc, size, typ))
	
	attribs <- forM [0..numA-1] $ \index -> do
		let i = fromIntegral index
		glGetActiveAttrib pid i maxlen nullPtr sptr tptr str
		name <- peekCString str
		loc <- glGetAttribLocation pid str
		size <- peek sptr
		typ <- peek tptr
		putStrLn . show $ (index, loc)
		return (name, (loc, size, typ))
	free str; free sptr; free tptr
	return (uniforms, attribs)

c_active_uniform_max_length = 0x8B87
c_active_attribute_max_length = 0x8B8A
c_active_uniforms = 0x8B86
c_active_attributes = 0x8B89


-- ** Uniform

-- (location, length of array or 1, ptr)
newtype Uniform p a = Uniform (GLint, GLsizei, Ptr ())

-- 
class UnifVal a where
	glUniform :: (GLint, GLsizei, Ptr ()) -> a -> GL ()

--class GLVar m v a where
--	($=) :: m p a -> a -> (m (), v ())
--	($-) :: m p a -> v a -> (m (), v ())
--instance UnifVal a => GLVar Uniform UniformValue a where
--	unif $= value = unif $- unifVal value
--	unif $- value = (coerce unif, coerce value)
--instance AttrStruct a => GLVar Attrib Buffer a where
--	attr $= value = attr $- buffer "tmp" value
--	attr $- buffer = (coerce attr, coerce buffer)
-- UnifVal a => (Uniform p a, a)
-- UnifStruct a => (UniformBlock p a, Buffer a)
-- GLStruct? std130?


-- ** Attrib

-- (index, size, normalize, divisor)
newtype Attrib p a = Attrib (GLuint, GLsizei, GLboolean, GLuint) deriving Show

class (Num a, Storable a) => GenericVertexAttribute a where
	glVertexAttrib4v :: GLuint -> Ptr (V4 a) -> GL ()

instance GenericVertexAttribute Float where
	glVertexAttrib4v idx = glVertexAttrib4fv idx . castPtr
instance GenericVertexAttribute Int32 where
	glVertexAttrib4v idx = glVertexAttribI4iv idx . castPtr
instance GenericVertexAttribute Word32 where
	glVertexAttrib4v idx = glVertexAttribI4uiv idx . castPtr

class ShaderAttribute a where
	glVertexAttrib :: GLuint -> a -> GL ()

class Storable b => AttrStruct b a p | a -> p where
	glVertexAttribPtr :: a -> Buffer b -> GL ()


-- ** Vertex Array Object

-- (glo, init)
newtype VertexArray p = VertexArray (GLO, GL ())


-- ** Vertex Picker

newtype VertexPicker = VertexPicker (GLenum -> GL Bool)

class VertexIx a where
	vxix :: m a -> (GLenum, GLint)
instance VertexIx Word8 where
	vxix _ = (0x1401, 1)
instance VertexIx Word16 where
	vxix _ = (0x1403, 2)
instance VertexIx Word32 where
	vxix _ = (0x1405, 4)


-- ** Draw Operation

newtype ClearBufferMask = ClearBufferMask GLenum deriving Num

-- [MainThread, GLThread]
-- if Nothing, main GL thread should stop before the next frame.
drawOrExit :: IORef (Maybe (GL ())) -- eglSwapBuffer inside
drawOrExit = unsafePerformIO $ newIORef Nothing

drawQueue :: Chan (GL ())
drawQueue = unsafePerformIO newChan
{-# NOINLINE drawQueue #-}


-- ** Framebuffer

data Framebuffer = Framebuffer (IORef (V2 GLsizei)) GLO
data Renderbuffer a = Renderbuffer GLint GLenum (IORef (V2 GLsizei)) GLO

class Attachable a b where
	glAttachToFramebuffer :: GLenum -> a b -> IORef (V2 GLsizei) -> GL ()

defaultFramebuffer :: Framebuffer
defaultFramebuffer = unsafePerformIO $ do
	glo <- newIORef $ GLObj 0 undefined undefined
	dummy <- newIORef undefined
	return $ Framebuffer dummy glo

