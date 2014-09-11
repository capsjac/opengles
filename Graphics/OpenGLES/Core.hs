-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OpenGLES.Core
-- Copyright   :  (c) capsjac 2014
-- License     :  GPLv3 (see the file LICENSE)
-- 
-- The neat and easy to use wrapper for OpenGL EmbedSystems (ES).
-- The wrapper is optimised for mobile and have small footprint.
-- Assuming OpenGL ES 2.0 or any later version, however, also works
-- with OpenGL 4.1/4.3+ on desktop.
--
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.OpenGLES.Core where
import Control.Applicative
import Control.Monad
import Control.Concurrent (forkOS, ThreadId)
import Control.Concurrent.Chan
import Control.Exception (catch, SomeException)
import Data.Array.Base (getNumElements)
import Data.Array.Storable
import Data.Array.Storable.Internals
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.Coerce (coerce)
import Data.IORef
import Data.Typeable
import Foreign hiding (newForeignPtr, addForeignPtrFinalizer)
import Foreign.C.String (peekCString, peekCStringLen)
import Foreign.Concurrent (newForeignPtr, addForeignPtrFinalizer)
-- GHC only   ^^^^^^^^^^
import Graphics.OpenGLES.Base
import Graphics.OpenGLES.Env
import System.IO.Unsafe (unsafePerformIO)


-- * Initialization

forkGL
	:: IO () 
	-> GL () 
	-> IO ThreadId 
	               
forkGL bindEGL unbindEGL = forkOS $ do
	let loop count = do
		bindEGL
		putStrLn "bindEGL"
		readChan drawQueue >>= id
		putStrLn "unbindEGL"
		unbindEGL
		loop (count + 1)
	catch (loop 0) $ \(e :: SomeException) -> do
		glLog $ "Rendering thread terminated: " ++ show e
		unbindEGL
		modifyIORef contextRev (+1)
glRun :: GL () -> IO ()
glRun io = writeChan drawQueue io


glRunRes :: GL () -> IO ()
glRunRes io = writeChan drawQueue io


glLog :: String -> IO ()
glLog msg = writeChan errorQueue msg


readGLLog :: IO [String]
readGLLog = do
	empty <- isEmptyChan errorQueue
	if empty
		then return []
		else (:) <$> readChan errorQueue <*> readGLLog


getGLLogContents :: IO [String]
getGLLogContents = getChanContents errorQueue


-- * Buffering

-- Buffer usage [GLtype] stride id (latestArray, isBufferSynced)
data Buffer a = Buffer BufferUsage ObjId (IORef (StorableArray Int a, Bool))


-- ** Constructing mutable Buffers


newBuffer :: Storable a => BufferUsage -> Int -> IO (Buffer a)
newBuffer usage elems = do
	array <- newArray_ (0, elems)
	Buffer usage <$> newObjId <*> newIORef (array, False)


newListBuffer
	:: Storable a => BufferUsage -> (Int, Int) -> [a] -> IO (Buffer a)
newListBuffer usage ix xs = do
	array <- newListArray ix xs
	Buffer usage <$> newObjId <*> newIORef (array, False)


loadBuffer :: forall a. Storable a => BufferUsage -> B.ByteString -> IO (Buffer a)
loadBuffer usage bs@(B.PS foreignPtr offset length) = do
	let fp | offset == 0 = foreignPtr
	       | otherwise = case B.copy bs of (B.PS f _ _) -> f
	let elems = (length `div` sizeOf (undefined :: a))
	let array = StorableArray 0 (elems-1) elems (castForeignPtr fp)
	Buffer usage <$> newObjId <*> newIORef (array, False)


-- ** Updating mutable Buffers


getSArray :: Storable a => Buffer a -> IO (StorableArray Int a)
getSArray (Buffer _ _ ref) = readIORef ref >>= return . fst

bindBuf :: GLenum -> Buffer a -> GL ()
bindBuf target (Buffer usage obj aref) = do
	rev <- readIORef contextRev
	buf <- readIORef obj >>= \case
		Ok gen buf _ | gen == rev ->
			return buf
		otherwise -> do
			modifyIORef aref $ \(arr, _) -> (arr, False)
			genObj rev obj glGenBuffers glDeleteBuffers
	glBindBuffer target buf

-- | 
commit :: forall a. Storable a => Buffer a -> IO ()
commit buf@(Buffer (BufferUsage usage) _ aref) = do
	bindBuf c_array_buffer buf
	(array, synced) <- readIORef aref
	when (not synced) $ do
		size <- (sizeOf (undefined :: a) *) <$> getNumElements array
		withStorableArray array $ \ptr ->
			glBufferData c_array_buffer size (castPtr ptr) usage
{-renewBuffer :: Storable a => BufferUsage -> Int -> Buffer a -> IO (Buffer a)
renewBuffer usage elems (Buffer _ old _) = do
	array <- newArray_ (0, elems)
	rev <- readIORef contextRev
	objid <- readIORef old >>= \case
		Ok gen id | gen == rev -> newIORef (Renew id)
		otherwise -> newObjId
	Buffer usage objid <$> newIORef (array, False)

renewListBuffer
	:: Storable a =>
	BufferUsage -> (Int, Int) -> [a] -> Buffer b -> IO (Buffer a)
renewListBuffer usage ix xs (Buffer _ old _) = do
	array <- newListArray ix xs
	rev <- readIORef contextRev
	objid <- readIORef old >>= \case
		Ok gen id | gen == rev -> newIORef (Renew id)
		otherwise -> newObjId
	Buffer usage objid <$> newIORef (array, False)
-}

newtype BufferUsage = BufferUsage GLenum
-- | STATIC_DRAW (Default)
app2gl = BufferUsage 0x88E4
-- | DYNAMIC_DRAW
app2glDyn = BufferUsage 0x88E8
-- | STREAM_DRAW
app2glStream = BufferUsage 0x88E0
-- *** GL ES 3+
-- | STATIC_READ
gl2app = BufferUsage 0x88E5
-- | DYNAMIC_READ
gl2appDyn = BufferUsage 0x88E9
-- | STREAM_READ
gl2appStream = BufferUsage 0x88E1
-- | STATIC_COPY
gl2gl = BufferUsage 0x88E6
-- | DYNAMIC_COPY
gl2glDyn = BufferUsage 0x88EA
-- | STREAM_COPY
gl2glStream = BufferUsage 0x88E2


-- ** Raw Buffer Operations

bindBuffer :: BufferSlot -> Buffer a -> GL ()
bindBuffer (BufferSlot target) buf =
	glBindBuffer target =<< getBufId buf

bindBufferRange :: BufferSlot -> GLuint -> Buffer a -> Int -> Int -> GL ()
bindBufferRange (BufferSlot t) index buf offset size =
	(\x -> glBindBufferRange t index x offset size) =<< getBufId buf

bindBufferBase :: BufferSlot -> GLuint -> Buffer a -> GL ()
bindBufferBase (BufferSlot t) index buf =
	glBindBufferBase t index =<< getBufId buf

bufferData :: BufferSlot -> BufferUsage -> (Ptr a, Int) -> GL ()
bufferData (BufferSlot target) (BufferUsage usage) (ptr, size) =
	glBufferData target size (castPtr ptr) usage

bufferSubData :: BufferSlot -> Int -> (Ptr a, Int) -> GL ()
bufferSubData (BufferSlot target) offset (ptr, size) =
	glBufferSubData target offset size (castPtr ptr)

-- *** 3+ | GL_OES_mapbuffer glUnmapBufferOES
unmapBuffer :: BufferSlot -> GL Bool
unmapBuffer (BufferSlot target) =
	glUnmapBuffer target >>= return . (/= 0)

-- *** GL_OES_mapbuffer 
-- (*GL_APIENTRY glMapBufferOES (GLenum target, GLenum access);
-- define GL_WRITE_ONLY_OES                 0x88B9

-- *** 3+ | GL_EXT_map_buffer_range
-- glMapBufferRangeEXT glFlushMappedBufferRangeEXT
mapBufferRange :: BufferSlot -> Int -> Int -> GLbitfield -> GL (Ptr a)
mapBufferRange (BufferSlot target) offset size access =
	fmap castPtr $ glMapBufferRange target offset size access

flashMappedBufferRange :: BufferSlot -> Int -> Int -> GL ()
flashMappedBufferRange (BufferSlot target) offset size =
	glFlushMappedBufferRange target offset size

map_read_bit = 1 :: GLbitfield
map_write_bit = 2 :: GLbitfield
map_invalidate_range_bit = 4 :: GLbitfield
map_invalidate_buffer_bit = 8 :: GLbitfield
map_flush_explicit_bit = 16 :: GLbitfield
map_unsynchronized_bit = 32 :: GLbitfield

-- *** 3+ | GL_NV_copy_buffer glCopyBufferSubDataNV
copyBufferSubData :: BufferSlot -> BufferSlot -> Int -> Int -> Int -> GL ()
copyBufferSubData (BufferSlot read) (BufferSlot write) roffset woffset size =
	glCopyBufferSubData read write roffset woffset size

newtype BufferSlot = BufferSlot GLenum
array_buffer = BufferSlot 0x8892
element_array_buffer = BufferSlot 0x8893
-- *** 3+
pixel_pack_buffer = BufferSlot 0x88EB
pixel_unpack_buffer = BufferSlot 0x88EC
uniform_buffer = BufferSlot 0x8A11
transform_feedback_buffer = BufferSlot 0x8C8E
-- *** 3+ | GL_NV_copy_buffer
copy_read_buffer = BufferSlot 0x8F36
copy_write_buffer = BufferSlot 0x8F37
-- * Drawing

glDraw :: Typeable p
	=> DrawMode
	-> Program p
	-> [SetGraphicsState]
	-> [UniformAssignment p]
	-> VertexArray p
	-> VertexPicker
	-> IO Bool
glDraw (DrawMode mode) prog@(Program _ _ pref _) setState unifs
		(VertexArray setVA vref) (VertexPicker picker) = do
	rev <- readIORef contextRev
	pid <- readIORef pref >>= \case
		Ok generation pid _ | rev == generation -> return pid
		Making -> return 0
		Broken -> return 0
		otherwise -> do
			writeIORef pref Making
			glRun $ loadProgram prog (\_ _ _->nop)
			return 0
	if pid == 0 then return False
	else glRun (do
		case extVAO of
			Nothing -> setVA
			Just (gen, bind, del) -> readIORef vref >>= \case
				Ok gen vao _ | rev == gen ->
					bind vao
				otherwise ->
					genObj rev vref gen del >> setVA
		glUseProgram pid
		sequence setState
		sequence unifs
		picker mode
		Control.Monad.void $ validateProgram prog
		) >> return True

-- | See @Graphics.OpenGLES.State@
type SetGraphicsState = GL ()


-- ** Draw Mode


newtype DrawMode = DrawMode GLenum

drawPoints = DrawMode 0

drawLines = DrawMode 1

drawLineLoop = DrawMode 2

drawLineStrip = DrawMode 3

drawTriangles = DrawMode 4
triangleStrip = DrawMode 5

triangleFan = DrawMode 6


-- ** Programmable Shader

data Shader = Shader ShaderType GLName B.ByteString
	deriving Show
type ShaderType = GLenum--, String)

vertexShader, fragmentShader, pixelShader, computeShader, tessellationEvalS
	, tessellationCtrlS :: GLName -> B.ByteString -> Shader
vertexShader = Shader 0x8B31 --, "Vertex")
fragmentShader = Shader 0x8B30 --, "Fragment")
-- | Same as 'fragmentShader'
pixelShader = fragmentShader
-- | Compute shader requires /ES3.1+/
computeShader = Shader 0x91B9 --, "Compute")
-- | Geometry shader requires /GL_EXT_geometry_shader (ES3.1)/
geometryShader = Shader 0x8DD9
-- | Tessellation Shader requires /GL_EXT_tessellation_shader (ES3.1)/
tessellationEvalS = Shader 0x8E87 --, "TessellationEvalute")
tessellationCtrlS = Shader 0x8E88 --, "TessellationControl")



data Program p = Program
	{ programTF :: TransformFeedback
	, programShaders :: [Shader]
	, __pobj :: ObjId
	, programVariables :: ()
	} deriving Show

instance Show (IORef ObjState) where
	show = const "<id>"

mkProgram :: Typeable p => TransformFeedback -> [Shader] -> IO (Program p)
mkProgram tf shaders = Program tf shaders <$> newObjId <*> pure ()


data TransformFeedback =
	
	  NoFeedback
	
	| FeedbackArrays [String]
	
	| FeedbackPacked [String]
	deriving Show



glCompile :: Typeable p => Program p
          -> (Program p -> Int -> String -> Maybe ProgramBinary -> IO ())
          -> IO Bool 
glCompile prog@(Program _ _ ref _) progressLogger = do
	putStrLn "glCompile"
	rev <- readIORef contextRev
	readIORef ref >>= \case
		Ok generation id _ | rev == generation -> return True
		Making -> return False
		Broken -> return False
		Unused -> do
			writeIORef ref Making
			glRun $ loadProgram prog (progressLogger prog)
			return False

type ProgramBinary = (GLenum, B.ByteString)
-- ** Uniform Variable

--class GLVar m v a where
--	($=) :: m p a -> a -> (m (), v ())
--	($-) :: m p a -> v a -> (m (), v ())
--instance UnifVal a => GLVar Uniform UniformValue a where
--	unif $= value = unif $- unifVal value
--	unif $- value = (coerce unif, coerce value)
--instance AttrStruct a => GLVar Attrib Buffer a where
--	attr $= value = attr $- buffer "tmp" value
--	attr $- buffer = (coerce attr, coerce buffer)
type UniformAssignment p = GL ()

--UnifVal a => (Uniform p a, a)

newtype Uniform p a = Uniform GLint

uniform :: UnifVal a => GLName -> IO (Uniform p a)
uniform name = return $ Uniform 0
--withCString name (glGetUniformLocation prog)


class UnifVal a where
	glUniform :: GLint -> a -> IO ()


-- ** Vertex Attribute

newtype Attrib p a = Attrib (GLint, GLboolean)

attrib :: AttrStruct a => Program p -> GLName -> IO (Attrib p a)
attrib prog name = return $ Attrib (0, 0)
	{-readIORef (_pobj prog) >>= \case
		Ok generation id -> if ...
			withCString name (glGetAttribLocation prog)
			typecheck...
			Attrib (loc, 0)
		Unused -> glCompile ...
		Making -> wait...
		Broken -> return $ Attrib (name, 0)-}

normalized :: Attrib p a -> Attrib p a
normalized (Attrib (n, _)) = Attrib (n, 1)

class AttrStruct a where
	glAttrib :: GLint -> GLboolean -> Buffer a -> IO ()

type SetVertexAttr p = GL ()
--($=) :: Attrib p a -> Buffer a -> SetVertexAttr p
--Attrib loc 2 $= buf = glVertexAttribIPointer
--Attrib loc norm $= buf = do
--	glBindBuffer c_array_buffer =<< getBufId buf
--	4eachstruct:
--	glVertexAttribPointer loc size typ norm stride (idToPtr id)

c_array_buffer = 0x8892

data VertexArray p = VertexArray (GL ()) ObjId

newVA :: [SetVertexAttr p] -> IO (VertexArray p)
newVA attrs = VertexArray (sequence_ attrs) <$> newObjId


-- ** Vertex Picker


newtype VertexPicker = VertexPicker (GLenum -> GL ())


-- Wrapping glDrawArrays
takeFrom :: Int32 -> Int32 -> VertexPicker
takeFrom first count =
	VertexPicker $ \mode -> do
		glDrawArrays mode first count
		showError "glDrawArrays"


-- Wrapping glDrawArraysInstanced[EXT]
takeFromInstanced :: Int32 -> Int32 -> Int32 -> VertexPicker
takeFromInstanced first count numInstances =
	VertexPicker $ \mode -> do
		glDrawArraysInstanced mode first count numInstances
		showError "glDrawArraysInstanced"
-- Wrapping glMultiDrawArraysEXT
takeFromMany :: [(Int32, Int32)] -> VertexPicker
takeFromMany list =
	VertexPicker $ \mode ->
		forM_ list $ \(first, count) -> do
			glDrawArrays mode first count
			showError "glDrawArrays[]"
		--	showError "glMultiDrawElementsEXT"
-- TakeFromManyRaw (Buffer Int32) (Buffer Word32)

class VertexIx a where
	glVxIx :: m a -> (GLenum, GLint)
instance VertexIx Word8 where
	glVxIx = const (0x1401, 1)
instance VertexIx Word16 where
	glVxIx = const (0x1403, 2)
instance VertexIx Word32 where
	glVxIx = const (0x1405, 4)

-- Wrapping glDrawElements
byIndex :: VertexIx a => (Buffer a) -> Int32 -> Int32 -> VertexPicker
byIndex buf first count =
	let (typ, stride) = glVxIx buf in
	VertexPicker $ \mode -> do
		-- bind GL_ELEMENT_ARRAY_BUFFER = 0x8893
		glBindBuffer 0x8893 =<< getBufId buf
		glDrawElements mode count typ (sizePtr $ first * stride)
		showError "glDrawElements"
-- Wrapping glDrawElementsInstanced[EXT]
byIndexInstanced :: VertexIx a => (Buffer a) -> Int32 -> Int32 -> Int32 -> VertexPicker
byIndexInstanced buf first count instances =
	let (typ, stride) = glVxIx buf in
	VertexPicker $ \mode -> do
		glBindBuffer 0x8893 =<< getBufId buf
		glDrawElementsInstanced mode count typ
			(sizePtr $ first * stride) instances
		showError "glDrawElementsInstanced"
-- Wrapping glMultiDrawElementsEXT
byIndices :: VertexIx a => (Buffer a) -> [(Int32, Int32)] -> VertexPicker
byIndices buf list =
	let (typ, stride) = glVxIx buf in
	VertexPicker $ \mode -> do
		glBindBuffer 0x8893 =<< getBufId buf
		forM_ list $ \(first, count) -> do
			glDrawElements mode count typ (sizePtr $ first * stride)
			showError "glDrawElements[]"
		
		--withFirstCountArray list $ \cptr iptr clen -> do
		--	glMultiDrawElementsEXT mode cptr typ iptr (clen * stride)
		--	showError "glMultiDrawElementsEXT"
-- ByIndicesRaw (Buffer w) (Buffer Word32) (Buffer Word32)


-- Wrapping glDrawRangeElements[EXT]
byIndexLimited :: VertexIx a => (Buffer a) -> Int32 -> Int32 -> Word32 -> Word32 -> VertexPicker
byIndexLimited buf first count min max =
	let (typ, stride) = glVxIx buf in
	VertexPicker $ \mode -> do
		glBindBuffer 0x8893 =<< getBufId buf
		glDrawElements mode count typ (sizePtr $ first * stride)
		showError "glDrawElements'"
		--showError "glDrawRangeElements[EXT]"
-- FromToIndexRaw !BufferRef !Int !Int !GLsizei !GLenum !Int


drawCallSequence :: [VertexPicker] -> VertexPicker
drawCallSequence xs =
	VertexPicker $ \mode ->
		mapM_ (\(VertexPicker f) -> f mode) xs



-- * Others

-- | @return ()@
nop :: Monad m => m ()
nop = return ()


-- @clear [] colorBuffer@
-- @clear [bindFramebuffer buf] (colorBuffer+depthBuffer)@
clear
	:: [SetGraphicsState]
	-> ClearBuffers
	-> IO ()
clear gs (ClearBufferFlags flags) = glRun (sequence gs >> glClear flags)

newtype ClearBuffers = ClearBufferFlags GLenum deriving Num
clearDepth = ClearBufferFlags 0x100
clearStencil = ClearBufferFlags 0x400
clearColor = ClearBufferFlags 0x4000

flushCommandQ :: IO ()
flushCommandQ = glRun glFlush

finishCommands :: IO ()
finishCommands = glRun glFinish
-- * Internal


contextRev :: IORef Int
contextRev = unsafePerformIO $ newIORef 0

drawQueue :: Chan (GL ())
drawQueue = unsafePerformIO newChan
{-# NOINLINE drawQueue #-}

errorQueue :: Chan String
errorQueue = unsafePerformIO newChan
{-# NOINLINE errorQueue #-}

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

showError :: String -> GL ()
showError location =
	getError >>= maybe nop
		(\err -> glLog $ location ++ ": " ++ show err)


data ObjState =
	-- gen, object id, fp
	  Ok Int GLuint (ForeignPtr GLuint)
	| Making
	| Unused
	| Broken

type ObjId = IORef ObjState
newObjId = newIORef Unused

getBufId :: Buffer a -> GL GLuint
getBufId (Buffer _ ref _) = do
	rev <- readIORef contextRev
	readIORef ref >>= \case
		Ok gen id _ | rev == gen -> return id
		otherwise -> return 0

sizePtr :: Int32 -> Ptr ()
sizePtr = intPtrToPtr . fromIntegral


-- ** Loading Shaders

loadProgram
	:: Typeable p => Program p
	-> (Int -> String -> Maybe ProgramBinary -> GL ())
	-> GL ()
loadProgram prog@(Program tf shaders ref vars) progressLogger = do
	let numShaders = length shaders
	let progname = show (typeRep prog)
	let msg = "Start compiling: " ++ progname
	glLog msg
	progressLogger 0 msg Nothing
	
	pid <- glCreateProgram
	if pid == 0 then do
		showError "glCreateProgram"
		progressLogger (numShaders + 1)
			"Fatal: glCreateProgram returned 0." Nothing
		writeIORef ref Broken
	else do
		
		results <- mapM (loadShader progressLogger) (zip [1..] shaders)
		-- putStrLn $ show results
		if any (== Nothing) results
		then writeIORef ref Broken
		else do
			forM_ results $ \(Just sid) -> do
				glAttachShader pid sid
				showError "glAttachShader"
			glLinkProgram pid
			showError "glLinkProgram"
			postLink progname numShaders ref () pid progressLogger
		sequence_ [glDeleteShader s | Just s <- results]
	glLog "---------------"

postLink :: String -> Int -> ObjId -> () -> GLuint
	-> (Int -> String -> Maybe ProgramBinary -> GL ()) -> GL ()
postLink progname numShaders ref vars pid
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
		writeIORef ref Broken
	else do
		-- obtain shader variables
		putStrLn.show=<<getActiveVariables pid
		--writeIORef vars (,)
		rev <- readIORef contextRev
		fp <- newForeignPtr nullPtr (glDeleteProgram pid)
		writeIORef ref $ Ok rev pid fp
		let msg = "Sucessfully linked " ++ progname ++ "!" ++ info'
		glLog msg
		progressLogger (numShaders + 1) msg Nothing

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
	-> GL (Maybe GLuint)
loadShader progressLogger (i, Shader shaderType name bs) = do
	sid <- glCreateShader shaderType
	if sid == 0 then do
		showError "glCreateShader"
		let msg = "Fatal: glCreateShader returned 0."
		glLog msg
		progressLogger i msg Nothing
		return Nothing
	else do
		B.useAsCString bs $ \src -> do
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
						return Nothing
					else do
						let msg = name ++ " ... done" ++ info'
						glLog msg
						progressLogger i msg Nothing
						return $ Just sid

c_compile_status = 0x8B81

--getActiveVariables :: GLuint -> GL []
getActiveVariables pid = do
	sptr <- malloc
	glGetProgramiv pid c_active_uniform_max_length sptr
	uMaxLen <- peek sptr
	glGetProgramiv pid c_active_attribute_max_length sptr
	aMaxLen <- peek sptr
	let maxlen = max uMaxLen aMaxLen
	str <- mallocBytes (fromIntegral maxlen)
	
	glGetProgramiv pid c_active_uniforms sptr
	numU <- fromIntegral <$> peek sptr
	glGetProgramiv pid c_active_attributes sptr
	numA <- fromIntegral <$> peek sptr
	
	tptr <- malloc
	uniforms <- forM [0..numU-1] $ \ index -> do
		glGetActiveUniform pid index maxlen nullPtr sptr tptr str
		name <- peekCString str
		size <- peek sptr
		typ <- peek tptr
		return (name, size, typ)
	
	attribs <- forM [0..numA-1] $ \index -> do
		glGetActiveAttrib pid index maxlen nullPtr sptr tptr str
		name <- peekCString str
		size <- peek sptr
		typ <- peek tptr
		return (name, size, typ)
	free str; free sptr; free tptr
	return (uniforms, attribs)

c_active_uniform_max_length = 0x8B87
c_active_attribute_max_length = 0x8B8A
c_active_uniforms = 0x8B86
c_active_attributes = 0x8B89

validateProgram :: Program p -> GL String
validateProgram prog = alloca $ \intptr -> do
	Ok _ pid _ <- readIORef $ __pobj prog
	glValidateProgram pid
	glGetProgramiv pid c_info_log_length intptr
	len <- fmap fromIntegral $ peek intptr
	info <- allocaBytes len $ \buf -> do
		glGetProgramInfoLog pid (fromIntegral len) nullPtr buf
		peekCStringLen (buf, len-1)
	glLog $ "validateProgram:\n" ++ info
	return info


-- ** Garbage collection for GPU objects

-- | genObj rev objref glGenBuffers glDeleteBuffers
genObj :: Int -> ObjId -> (GLsizei -> Ptr GLuint -> GL ())
		-> (GLsizei -> Ptr GLuint -> GL ()) -> GL GLuint
genObj rev ref genSomeObjs delSomeObjs = do
	fp <- mallocForeignPtr
	withForeignPtr fp $ \ptr -> do
		genSomeObjs 1 ptr
		showError "genObj"
		obj <- peek ptr
		writeIORef ref $ Ok rev obj fp
		
		addForeignPtrFinalizer fp $ do
			currentRev <- readIORef contextRev
			readIORef ref >>= \case
				Ok gen obj _ | gen == currentRev -> do
					with obj $ \ptr -> delSomeObjs 1 ptr
					showError "delObj"
				otherwise -> glLog "object killed"
		return obj

