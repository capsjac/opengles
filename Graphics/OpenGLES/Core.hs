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

module Graphics.OpenGLES.Core where
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import qualified Data.ByteString as B
import Data.Coerce
import Data.IORef
import Data.Typeable
import Foreign
import Foreign.C.String
import Graphics.OpenGLES.Base
import System.IO.Unsafe


-- * Initialization

forkGL
	:: IO ()
	-> GL ()
	-> IO ThreadId
forkGL bindEGL unbindEGL = forkOS $ do
	let loop count = do
		bindEGL
		readChan drawQueue
		unbindEGL
		loop (count + 1)
	loop 0 `onException` unbindEGL

glRun :: GL () -> IO ()
glRun io = writeChan drawQueue io

resRun :: GL () -> IO ()
resRun io = writeChan drawQueue io

readGLErrors :: IO [String]
readGLErrors = do
	empty <- isEmptyChan errorQueue
	if empty
		then return []
		else (:) <$> readChan errorQueue <*> readGLErrors

getErrorContents :: IO [String]
getErrorContents = getChanContents errorQueue


-- * Buffering

-- Buffer [GLtype] stride (isBufferSynced, bytes)
data Buffer a = Buffer [GLenum] Int32 (IORef (GLuint, Bool))

newtype BufferUsage = BufferUsage GLenum
-- | STATIC_DRAW
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

draw :: Program p
	=> DrawMode
	-> p
	-> SetGraphicsState
	-- Uniform p a
	-- Attrib p a
	-> VertexPicker
	-> GL ()
draw (DrawMode mode) prog gs (VertexPicker picker) = do
	gs
	picker mode

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
type ShaderType = (GLenum, String)

vertexShader, fragmentShader, pixelShader, computeShader, tessellationEvalS
	, tessellationCtrlS :: GLName -> B.ByteString -> Shader
vertexShader = Shader (0x8B31, "Vertex")
fragmentShader = Shader (0x8B30, "Fragment")
pixelShader = fragmentShader -- ^ Same as 'fragmentShader'
computeShader = Shader (0x91B9, "Compute")
-- | Tessellation Shader requires /ES3.1+/ and /GL_EXT_tessellation_shader/
tessellationEvalS = Shader (0x8E87, "TessellationEvalute")
tessellationCtrlS = Shader (0x8E88, "TessellationControl")

class Typeable p => Program p where
	programName :: p -> IO String
	programName _ = show $ typeRep (Proxy :: Proxy p)

	srcShaders :: p -> IO [Shader]
	recordVars :: p -> IO TransformFeedback

data TransformFeedback =
	  NoFeedback
	| FeedbackArrays [String]
	| FeedbackPacked [String]


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
instance VertexIx Word8
instance VertexIx Word16
instance VertexIx Word32

-- Wrapping glDrawElements
byIndex :: VertexIx a => (Buffer a) -> Int32 -> Int32 -> VertexPicker
byIndex buf@(Buffer [typ] stride _) first count =
	VertexPicker $ \mode -> do
		-- bind GL_ELEMENT_ARRAY_BUFFER = 0x8893
		glBindBuffer 0x8893 =<< getBufId buf
		glDrawElements mode count typ (sizePtr $ first * stride)
		showError "glDrawElements"

-- Wrapping glDrawElementsInstanced[EXT]
byIndexInstanced :: VertexIx a => (Buffer a) -> Int32 -> Int32 -> Int32 -> VertexPicker
byIndexInstanced buf@(Buffer [typ] stride _) first count instances =
	VertexPicker $ \mode -> do
		glBindBuffer 0x8893 =<< getBufId buf
		glDrawElementsInstanced mode count typ
			(sizePtr $ first * stride) instances
		showError "glDrawElementsInstanced"

-- Wrapping glMultiDrawElementsEXT
byIndices :: VertexIx a => (Buffer a) -> [(Int32, Int32)] -> VertexPicker
byIndices buf@(Buffer [typ] stride _) list =
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
byIndexLimited buf@(Buffer [typ] stride _) first count min max =
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
nop :: m ()
nop = return ()

clear
	:: SetGraphicsState
	-> ClearBuffers
	-> IO ()
clear gs (ClearBufferFlags flags) = glRun (gs >> glClear flags)

newtype ClearBuffers = ClearBufferFlags Int32 deriving Show
clearDepth = ClearBufferFlags 0x100
clearStencil = ClearBufferFlags 0x400
clearColor = ClearBufferFlags 0x4000

flushCommandQ :: IO ()
flushCommandQ = glRun glFlush

finishCommands :: IO ()
finishCommands = glRun glFinish



-- * Internal

drawQueue :: Chan (GL ())
drawQueue = unsafePerformIO newChan

errorQueue :: Chan String
errorQueue = unsafePerformIO newChan

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
	getError >>= maybe (return ()) (\err ->
		writeChan errorQueue $ location ++ ": " ++ show err)

getBufId :: Buffer a -> IO GLuint
getBufId (Buffer _ _ ref) = readIORef ref >>= return . fst

sizePtr :: Int32 -> Ptr ()
sizePtr = intPtrToPtr . fromIntegral
