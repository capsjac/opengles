-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OpenGLES.Core
-- Copyright   :  (c) capsjac 2014
-- License     :  LGPL-3 (see the file LICENSE)
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
{-# LANGUAGE CPP #-}

module Graphics.OpenGLES.Core where
import Control.Applicative
import Control.Monad
import Control.Concurrent (forkOS, ThreadId, myThreadId, killThread)
import Control.Concurrent.Chan
import Control.Exception (catch, SomeException)
import Control.Future
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
import Graphics.OpenGLES.Types
import System.IO.Unsafe (unsafePerformIO)


-- * Initialization

forkGL
	:: IO Bool
	-> GL ()
	-> GL ()
	-> IO ThreadId
forkGL resumeGL suspendGL swapBuffers = forkOS $ do
	writeIORef drawOrExit (Just swapBuffers)
	resumeGL -- Note: implicit glFlush here
	putStrLn "bindEGL"
	-- glRestoreLostObjects
	let loop count = do
		putStrLn $ "start draw " ++ show count
		readChan drawQueue >>= id
		loop (count + 1)
	catch (loop 0) $ \(e :: SomeException) -> do
		glLog $ "Rendering thread terminated: " ++ show e
		suspendGL
		putStrLn "unbindEGL"
		writeIORef drawOrExit (Just (glLog "Fatal lifecycle bug"))

stopGL :: IO ()
stopGL = do
	putStrLn "stopGL"
	writeIORef drawOrExit Nothing
	let waitGLThread = readIORef drawOrExit >>= \case
		Just _ -> nop
		Nothing -> waitGLThread
	waitGLThread
	putStrLn "Rendering has stopped."

--destroyGL :: IO ()
--destroyGL = runGL $ eglMakeCurrent Nothing, eglDestroyXXX ...

endFrameGL :: IO ()
endFrameGL = runGL $ do
	readIORef drawOrExit >>= \case
		Just eglSwapBuffer -> eglSwapBuffer
		Nothing -> myThreadId >>= killThread

runGL :: GL () -> IO ()
runGL io = writeChan drawQueue io

--runGLRes :: GL () -> IO ()
--runGLRes io = forkOS

withGL :: GL a -> IO (Future' a)
withGL io = asyncIO $ \update -> runGL (io >>= update . Finished)

-- | drawQueue may have drawcalls that use previous context,
-- so make it sure they are removed from the queue.
resetDrawQueue :: IO ()
resetDrawQueue = do
	empty <- isEmptyChan drawQueue
	when (not empty) (readChan drawQueue >> resetDrawQueue)

glLog :: String -> IO ()
glLog msg = writeChan errorQueue msg

glReadLogs :: IO [String]
glReadLogs = do
	empty <- isEmptyChan errorQueue
	if empty
		then return []
		else (:) <$> readChan errorQueue <*> glReadLogs

glLogContents :: IO [String]
glLogContents = getChanContents errorQueue


-- * Buffering

type GLArray a = StorableArray Int a
-- forall s. ST s a -> a
-- Buffer usage [GLtype] stride id (latestArray, isBufferSynced)
data Buffer a =
	Buffer BufferUsage GLO (IORef (GLArray a))
-- DoubleBuffer BufferUsage GLO GLO (IORef (Bool, GLArray a, GLArray a))


-- ** Constructing mutable Buffers

newGLO
	:: (GLsizei -> Ptr GLuint -> GL ())
	-> (GLuint -> GL ())
	-> (GLsizei -> Ptr GLuint -> GL ())
	-> GL GLO
newGLO gen bind del = do
	ref <- newIORef undefined
	genObj ref gen bind del
	return ref

-- | genObj glo glGenBuffers glDeleteBuffers
genObj
	:: GLO
	-> (GLsizei -> Ptr GLuint -> GL ())
	-> (GLuint -> GL ())
	-> (GLsizei -> Ptr GLuint -> GL ())
	-> GL GLuint
genObj ref genObjs bindObj delObjs = do
	fp <- mallocForeignPtr
	withForeignPtr fp $ \ptr -> do
		genObjs 1 ptr
		showError "genObj"
		obj <- peek ptr
		writeIORef ref (obj, fp)
		addForeignPtrFinalizer fp $ do
			(obj, _) <- readIORef ref
			with obj $ \ptr -> do
				delObjs 1 ptr
				showError "delObj"
		bindObj obj
		return obj
newBuffer = newGLO glGenBuffers (glBindBuffer 0x8892) glDeleteBuffers

glNewBuffer :: forall a. Storable a => BufferUsage -> Int -> GL (Buffer a)
glNewBuffer usage elems = do
	array <- newArray_ (0, elems)
	glo <- newBuffer
	bufferData array_buffer usage (elems * sizeOf (undefined :: a)) nullPtr
	Buffer usage glo <$> newIORef array

glNewListBuffer
	:: forall a. Storable a => BufferUsage
	-> (Int, Int)
	-> [a]
	-> GL (Buffer a)
glNewListBuffer usage ix xs = do
	array <- newListArray ix xs
	glo <- newBuffer
	withStorableArraySize array $ \size ptr ->
		bufferData array_buffer usage size ptr
	Buffer usage glo <$> newIORef array

glLoadBS :: forall a. Storable a => BufferUsage -> B.ByteString -> GL (Buffer a)
glLoadBS usage bs@(B.PS foreignPtr offset len) = do
	let fp | offset == 0 = foreignPtr
	       | otherwise = case B.copy bs of (B.PS f _ _) -> f
	let elems = (len `div` sizeOf (undefined :: a))
	let array = StorableArray 0 (elems-1) elems (castForeignPtr fp)
	glo <- newBuffer
	withForeignPtr fp $ \ptr ->
		bufferData array_buffer usage len ptr
	Buffer usage glo <$> newIORef array


-- ** Updating mutable Buffers

unsafeObtainStorableArray :: Buffer a -> IO (GLArray a)
unsafeObtainStorableArray (Buffer _ _ arr) = readIORef arr

withStorableArraySize
	:: forall i e a. Storable e
	=> StorableArray i e -> (Int -> Ptr e -> IO a) -> IO a
withStorableArraySize (StorableArray _ _ n fp) f =
	withForeignPtr fp (f size)
	where size = n * sizeOf (undefined :: e)

-- Performance hint http://www.opentk.com/node/1930
glRenewBuffer :: forall a. Storable a => BufferUsage -> Int -> Buffer a -> GL ()
glRenewBuffer usage elems buf@(Buffer _ _ ref) = do
	array <- newArray_ (0, elems)
	bindBuffer array_buffer buf
	bufferData array_buffer usage (elems * sizeOf (undefined :: a)) nullPtr
	writeIORef ref array

glRenewListBuffer :: forall a. Storable a =>
	BufferUsage -> (Int, Int) -> [a] -> Buffer a -> GL ()
glRenewListBuffer usage ix xs buf@(Buffer _ _ ref) = do
	array <- newListArray ix xs
	bindBuffer array_buffer buf
	withStorableArraySize array $ \size ptr -> do
		bufferData array_buffer usage 0 nullPtr
		bufferData array_buffer usage size ptr
	writeIORef ref array

glReloadBS :: forall a. Storable a =>
	BufferUsage -> B.ByteString -> Buffer a -> GL ()
glReloadBS usage bs@(B.PS foreignPtr offset len) buf = do
	let fp | offset == 0 = foreignPtr
	       | otherwise = case B.copy bs of (B.PS f _ _) -> f
	let elems = (len `div` sizeOf (undefined :: a))
	let array = StorableArray 0 (elems-1) elems (castForeignPtr fp)
	withForeignPtr fp $ \ptr -> do
		bufferData array_buffer usage 0 nullPtr
		bufferData array_buffer usage len ptr


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
bindBuffer (BufferSlot target) (Buffer _ glo _) =
	glBindBuffer target . fst =<< readIORef glo

bindBufferRange :: BufferSlot -> GLuint -> Buffer a -> Int -> Int -> GL ()
bindBufferRange (BufferSlot t) index (Buffer _ glo _) offset size = do
	buf <- fmap fst $ readIORef glo
	glBindBufferRange t index buf offset size

bindBufferBase :: BufferSlot -> GLuint -> Buffer a -> GL ()
bindBufferBase (BufferSlot t) index (Buffer _ glo _) = do
	glBindBufferBase t index . fst =<< readIORef glo

bufferData :: BufferSlot -> BufferUsage -> Int -> Ptr a -> GL ()
bufferData (BufferSlot target) (BufferUsage usage) size ptr =
	glBufferData target size (castPtr ptr) usage

bufferSubData :: BufferSlot -> Int -> Int -> Ptr a -> GL ()
bufferSubData (BufferSlot target) offset size ptr =
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
	-> [GraphicsState]
	-> [UniformAssignment p]
	-> VertexArray p
	-> VertexPicker
	-> GL Bool
glDraw (DrawMode mode) prog@(Program pobj _ _ _) setState unifs
		(VertexArray (vao, setVA)) (VertexPicker picker) = do
	glUseProgram . fst =<< readIORef pobj
	sequence setState
	sequence unifs
	case extVAO of
		Nothing -> setVA
		Just (_, bind, _) -> readIORef vao >>= bind . fst
	picker mode
	glValidate prog
	return True

-- | See "Graphics.OpenGLES.State"
type GraphicsState = GL ()


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
	{ programGLO :: GLO
	, programTF :: TransformFeedback
	, programShaders :: [Shader]
	, programVariables :: ([GLVarDesc], [GLVarDesc])
	} deriving Show

type ProgramBinary = B.ByteString
-- | name: (location, length of array, type)
type GLVarDesc = (String, (GLint, GLsizei, GLenum))


-- 
glCompile
	:: Typeable p
	=> TransformFeedback
	-> [Shader]
	-> (Program p -> Int -> String -> Maybe ProgramBinary -> GL ())
	-> GL (Progress [String] (Program p))
glCompile tf shaders progressLogger = do
	glo <- newIORef undefined
	let prog = Program glo tf shaders ([],[])
	loadProgram prog (progressLogger prog)

data TransformFeedback =
	  NoFeedback
	| FeedbackArrays [String]
	| FeedbackPacked [String]
	deriving Show

-- 
--glCompile :: Typeable p => Program p
--         -> (Program p -> Int -> String -> Maybe ProgramBinary -> GL ())

-- | glValidateProgram checks to see whether the executables contained in
-- program can execute given the current OpenGL state.
glValidate :: Program p -> GL String
glValidate prog = alloca $ \intptr -> do
	(pid, _) <- readIORef $ programGLO prog
	glValidateProgram pid
	glGetProgramiv pid c_info_log_length intptr
	len <- fmap fromIntegral $ peek intptr
	info <- allocaBytes len $ \buf -> do
		glGetProgramInfoLog pid (fromIntegral len) nullPtr buf
		peekCStringLen (buf, len-1)
	glLog $ "validateProgram: " ++ info
	return info


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

-- UnifVal a => (Uniform p a, a)
-- UnifStruct a => (UniformBlock p a, Buffer a)
-- GLStruct? std130?
newtype Uniform p a = Uniform (GLint, GLsizei, Ptr ())
-- (location, length of array or 1, ptr)

uniform
	:: forall p a. (UnifVal a, Typeable p)
	=> GLName -> IO (Uniform p a)
uniform name = do
	desc <- lookupVarDesc typ
	case desc of
		Nothing -> return $ Uniform (-1, 0, nullPtr)
		Just (unifs, _) ->
			case lookup name unifs of
				Just unif -> validateType unif
				Nothing -> glLog errmsg >> return (Uniform (-1, 0, nullPtr))
	where
		typ = typeRep (undefined :: Program p)
		errmsg = "Uniform not found: " ++ name ++ " (" ++ show typ ++ ")"
		validateType (loc, size, gltyp) = do
			-- Prevent drawtime allocation (it leaks, though)
			ptr <- mallocArray (fromIntegral size) :: IO (Ptr Float)
			return $ Uniform (loc, size, castPtr ptr)

($=) :: UnifVal a => Uniform p a -> a -> UniformAssignment p
Uniform desc $= value = glUniform desc value

class UnifVal a where
	glUniform :: (GLint, GLsizei, Ptr ()) -> a -> GL ()

--instance UnifVal Float where
--	glUniform (loc, _, _) x = glUniform1f loc x

#define Uniform(_typ, _arg, _suffix, _rhs) \
instance UnifVal (_typ) where \
	glUniform (loc, _, _) _arg = glUniform/**/_suffix loc _rhs \

Uniform(Float,x,1f,x)
Uniform(Vec2,(V2 x y),2f,x y)
Uniform(Vec3,(V3 x y z),3f,x y z)
Uniform(Vec4,(V4 x y z w),4f,x y z w)
Uniform(Int32,x,1i,x)
Uniform(IVec2,(V2 x y),2i,x y)
Uniform(IVec3,(V3 x y z),3i,x y z)
Uniform(IVec4,(V4 x y z w),4i,x y z w)
Uniform(Word32,x,1ui,x)
Uniform(UVec2,(V2 x y),2ui,x y)
Uniform(UVec3,(V3 x y z),3ui,x y z)
Uniform(UVec4,(V4 x y z w),4ui,x y z w)

--instance UnifVal [Float] where
--	glUniform (loc, len, ptr) values = do
--		let len' = fromIntegral len
--		pokeArray (castPtr ptr :: Ptr Float) (take len' values)
--		glUniform1fv loc len (castPtr ptr)

pokeUniformArray
	:: Storable b => (GLint -> GLsizei -> Ptr a -> GL ())
	-> (GLint, GLsizei, Ptr ()) -> [b] -> GL ()
pokeUniformArray glUniformV (loc, len, ptr) values = do
	let len' = fromIntegral len
	pokeArray (castPtr ptr :: Ptr b) (take len' values)
	glUniformV loc len (castPtr ptr)

instance UnifVal [Float] where glUniform = pokeUniformArray glUniform1fv
instance UnifVal [Vec2] where glUniform = pokeUniformArray glUniform2fv
instance UnifVal [Vec3] where glUniform = pokeUniformArray glUniform3fv
instance UnifVal [Vec4] where glUniform = pokeUniformArray glUniform4fv
instance UnifVal [Int32] where glUniform = pokeUniformArray glUniform1iv
instance UnifVal [IVec2] where glUniform = pokeUniformArray glUniform2iv
instance UnifVal [IVec3] where glUniform = pokeUniformArray glUniform3iv
instance UnifVal [IVec4] where glUniform = pokeUniformArray glUniform4iv
instance UnifVal [Word32] where glUniform = pokeUniformArray glUniform1uiv
instance UnifVal [UVec2] where glUniform = pokeUniformArray glUniform2uiv
instance UnifVal [UVec3] where glUniform = pokeUniformArray glUniform3uiv
instance UnifVal [UVec4] where glUniform = pokeUniformArray glUniform4uiv

-- 'transpose' argument must be GL_FALSE in GL ES 2.0
pokeMatrix :: (Transpose a b, Storable b)
	=> (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
	-> (GLint, GLsizei, Ptr ()) -> a -> GL ()
pokeMatrix glUniformMatrixV (loc, _, ptr) matrix = do
	poke (castPtr ptr :: Ptr b) (transpose matrix)
	glUniformMatrixV loc 1 0 (castPtr ptr)

instance UnifVal Mat2 where glUniform = pokeMatrix glUniformMatrix2fv
instance UnifVal Mat3 where glUniform = pokeMatrix glUniformMatrix3fv
instance UnifVal Mat4 where glUniform = pokeMatrix glUniformMatrix4fv

-- GL ES 3.0+ supports transpose
pokeMatrixT :: Storable a
	=> (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
	-> (GLint, GLsizei, Ptr ()) -> a -> GL ()
pokeMatrixT glUniformMatrixV (loc, _, ptr) matrix = do
	poke (castPtr ptr :: Ptr a) matrix
	glUniformMatrixV loc 1 1 (castPtr ptr)

-- http://delphigl.de/glcapsviewer/gles_extensions.php 
instance UnifVal Mat2x3 where glUniform = pokeMatrixT glUniformMatrix2x3fv
instance UnifVal Mat2x4 where glUniform = pokeMatrixT glUniformMatrix2x4fv
instance UnifVal Mat3x2 where glUniform = pokeMatrixT glUniformMatrix3x2fv
instance UnifVal Mat3x4 where glUniform = pokeMatrixT glUniformMatrix3x4fv
instance UnifVal Mat4x2 where glUniform = pokeMatrixT glUniformMatrix4x2fv
instance UnifVal Mat4x3 where glUniform = pokeMatrixT glUniformMatrix4x3fv

-- 'transpose' argument must be GL_FALSE in GL ES 2.0
pokeMatrices :: (Transpose a b, Storable b)
	=> (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
	-> (GLint, GLsizei, Ptr ()) -> [a] -> GL ()
pokeMatrices glUniformMatrixV (loc, len, ptr) matrices = do
	let len' = fromIntegral len
	pokeArray (castPtr ptr :: Ptr b)
		(map transpose $ take len' matrices) -- maybe slow
	glUniformMatrixV loc len 0 (castPtr ptr)

instance UnifVal [Mat2] where glUniform = pokeMatrices glUniformMatrix2fv
instance UnifVal [Mat3] where glUniform = pokeMatrices glUniformMatrix3fv
instance UnifVal [Mat4] where glUniform = pokeMatrices glUniformMatrix4fv

-- GL ES 3.0+ supports transpose
pokeMatricesT :: Storable a
	=> (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
	-> (GLint, GLsizei, Ptr ()) -> [a] -> GL ()
pokeMatricesT glUniformMatrixV (loc, len, ptr) matrices = do
	let len' = fromIntegral len
	pokeArray (castPtr ptr :: Ptr a) (take len' matrices)
	glUniformMatrixV loc len 1 (castPtr ptr)

instance UnifVal [Mat2x3] where glUniform = pokeMatricesT glUniformMatrix2x3fv
instance UnifVal [Mat2x4] where glUniform = pokeMatricesT glUniformMatrix2x4fv
instance UnifVal [Mat3x2] where glUniform = pokeMatricesT glUniformMatrix3x2fv
instance UnifVal [Mat3x4] where glUniform = pokeMatricesT glUniformMatrix3x4fv
instance UnifVal [Mat4x2] where glUniform = pokeMatricesT glUniformMatrix4x2fv
instance UnifVal [Mat4x3] where glUniform = pokeMatricesT glUniformMatrix4x3fv


-- ** Vertex Attribute

newtype Attrib p a = Attrib (GLint, GLsizei, GLboolean, Int)
-- (location, size, normalize, divisor)
-- normalized color `divisor` 1 $= buffer

attrib
	:: forall p a. (AttrStruct a, Typeable p)
	=> GLName -> IO (Attrib p a)
attrib name = do
	desc <- lookupVarDesc typ
	case desc of
		Nothing -> return $ Attrib (-1, 0, 0, 0)
		Just (_, attrs) ->
			case lookup name attrs of
				Just attr -> validateType attr
				Nothing -> glLog errmsg >> return (Attrib (-1, 0, 0, 0))
	where
		typ = typeRep (undefined :: Program p)
		errmsg = "Attribute not found: " ++ name ++ " (" ++ show typ ++ ")"
		validateType (loc, size, gltyp) = return $ Attrib (loc, size, 0, 0)

normalized :: Attrib p a -> Attrib p a
normalized (Attrib (n, s, _, d)) = Attrib (n, s, 1, d)

class AttrStruct a where
	glAttrib :: GLint -> GLboolean -> Buffer a -> IO ()

type SetVertexAttr p = GL ()
--($=) :: Attrib p a -> Buffer a -> SetVertexAttr p
--Attrib loc 2 $= buf = glVertexAttribIPointer
--Attrib loc norm $= buf = do
--	glBindBuffer c_array_buffer =<< getBufId buf
--	4eachstruct:
--	glVertexAttribPointer loc size typ norm stride (idToPtr id)

newtype VertexArray p = VertexArray (GLO, GL ())
-- (glo, init)

-- TOOD: glDisableVertexAttribArray
glVA :: [SetVertexAttr p] -> GL (VertexArray p)
glVA attrs = do
	let setVA = sequence_ attrs
	glo <- case extVAO of
		Nothing -> return (error "GLO not used")
		Just (gen, bind, del) ->
			newGLO gen bind del <* setVA
	return $ VertexArray (glo, setVA)


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
		bindBuffer element_array_buffer buf
		glDrawElements mode count typ (sizePtr $ first * stride)
		showError "glDrawElements"

-- Wrapping glDrawElementsInstanced[EXT]
byIndexInstanced :: VertexIx a => (Buffer a) -> Int32 -> Int32 -> Int32 -> VertexPicker
byIndexInstanced buf first count instances =
	let (typ, stride) = glVxIx buf in
	VertexPicker $ \mode -> do
		bindBuffer element_array_buffer buf
		glDrawElementsInstanced mode count typ
			(sizePtr $ first * stride) instances
		showError "glDrawElementsInstanced"

-- Wrapping glMultiDrawElementsEXT
byIndices :: VertexIx a => (Buffer a) -> [(Int32, Int32)] -> VertexPicker
byIndices buf list =
	let (typ, stride) = glVxIx buf in
	VertexPicker $ \mode -> do
		bindBuffer element_array_buffer buf
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
		bindBuffer element_array_buffer buf
		glDrawElements mode count typ (sizePtr $ first * stride)
		showError "glDrawElements'"
		--showError "glDrawRangeElements[EXT]"
-- FromToIndexRaw !BufferRef !Int !Int !GLsizei !GLenum !Int

drawCallSequence :: [VertexPicker] -> VertexPicker
drawCallSequence xs =
	VertexPicker $ \mode ->
		mapM_ (\(VertexPicker f) -> f mode) xs



-- * Other Operations

-- |
-- > glClean [] colorBuffer
-- > glClean [bindFramebuffer buf] (colorBuffer+depthBuffer)
glClean
	:: [GraphicsState]
	-> ClearBuffers
	-> GL ()
glClean gs (ClearBufferFlags flags) = sequence gs >> glClear flags

newtype ClearBuffers = ClearBufferFlags GLenum deriving Num
clearDepth = ClearBufferFlags 0x100
clearStencil = ClearBufferFlags 0x400
clearColor = ClearBufferFlags 0x4000

flushCommandQ :: IO ()
flushCommandQ = runGL glFlush

finishCommands :: IO ()
finishCommands = runGL glFinish

-- | @return ()@
nop :: Monad m => m ()
nop = return ()

-- * Internal

-- [MainThread, GLThread]
-- if Nothing, main GL thread should stop before the next frame.
drawOrExit :: IORef (Maybe (GL ())) -- eglSwapBuffer inside
drawOrExit = unsafePerformIO $ newIORef Nothing

drawQueue :: Chan (GL ())
drawQueue = unsafePerformIO newChan
{-# NOINLINE drawQueue #-}

errorQueue :: Chan String
errorQueue = unsafePerformIO newChan
{-# NOINLINE errorQueue #-}


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

showError :: String -> GL ()
showError location = putStrLn location >>
	getError >>= maybe nop
		(\err -> glLog $ location ++ ": " ++ show err)


-- ** GL Object management

type GLO = IORef (GLuint, ForeignPtr GLuint)

instance Show GLO where
	show ref = show . unsafePerformIO $ readIORef ref

-- ** Garbage collection for GPU objects


sizePtr :: Int32 -> Ptr ()
sizePtr = intPtrToPtr . fromIntegral

-- glRestoreLostObjects :: GL ()
-- saveBuffer :: Buffer -> IO ()
-- saveBuffer buf = atomicModifyIORef' (buf:) bufferArchive
-- bufferArchive = unsafePerformIO $ newIORef []


-- ** Loading Shaders

-- binaryStore :: IORef [(String, B.ByteString)]
-- or (FilePath -> IO B.ByteString)
-- binaryStore = unsafePerformIO $ newIORef []

programDict :: IORef [(String, Program ())]
programDict = unsafePerformIO $ newIORef []

lookupVarDesc :: TypeRep -> IO (Maybe ([GLVarDesc], [GLVarDesc]))
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
		writeIORef (programGLO prog) (pid, fp)
		let msg = "Sucessfully linked " ++ progname ++ "!" ++ info'
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

getActiveVariables :: GLuint -> GL ([GLVarDesc], [GLVarDesc])
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
		return (name, (loc, size, typ))
	free str; free sptr; free tptr
	return (uniforms, attribs)

c_active_uniform_max_length = 0x8B87
c_active_attribute_max_length = 0x8B8A
c_active_uniforms = 0x8B86
c_active_attributes = 0x8B89


