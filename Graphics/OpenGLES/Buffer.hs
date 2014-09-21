{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.OpenGLES.Buffer (
  -- * Buffer
  -- ** Constructing Mutable Buffers
  Buffer,
  glNewBuffer, glLoadList, glLoadBS,
  -- ** Updating Mutable Buffers
  unsafeObtainStorableArray, withStorableArraySize,
  glRenewBuffer, glReloadList, glReloadBS,
  BufferUsage, app2gl, app2glDyn, app2glStream,
  gl2app, gl2appDyn, gl2appStream, gl2gl, gl2glDyn, gl2glStream,
  -- ** Raw Buffer Operations
  bindBuffer, bindBufferRange, bindBufferBase,
  bufferData, bufferSubData, 
  -- | /3+ | GL_OES_mapbuffer/ glUnmapBufferOES
  unmapBuffer,
  -- | /GL_OES_mapbuffer/
  -- (*GL_APIENTRY glMapBufferOES (GLenum target, GLenum access);
  -- define GL_WRITE_ONLY_OES                 0x88B9
  -- | /3+ | GL_EXT_map_buffer_range/
  -- glMapBufferRangeEXT glFlushMappedBufferRangeEXT
  mapBufferRange, flashMappedBufferRange,
  map_read_bit, map_write_bit, map_invalidate_range_bit,
  map_invalidate_buffer_bit, map_flush_explicit_bit, map_unsynchronized_bit,
  -- | /3+ | GL_NV_copy_buffer/ glCopyBufferSubDataNV
  copyBufferSubData,
  BufferSlot, array_buffer, element_array_buffer,
  pixel_pack_buffer, pixel_unpack_buffer,
  uniform_buffer, transform_feedback_buffer,
  copy_read_buffer, copy_write_buffer
  ) where
import Control.Applicative
import Data.Array.Base (getNumElements)
import Data.Array.Storable
import Data.Array.Storable.Internals
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.IORef
import Graphics.OpenGLES.Base
import Graphics.OpenGLES.Internal
import Foreign

-- ** Constructing Mutable Buffers

glNewBuffer :: forall a. Storable a => BufferUsage -> Int -> GL (Buffer a)
glNewBuffer usage elems = do
	array <- newArray_ (0, elems)
	glo <- newBuffer
	bufferData array_buffer usage (elems * sizeOf (undefined :: a)) nullPtr
	Buffer usage glo <$> newIORef array

glLoadList
	:: forall a. Storable a => BufferUsage
	-> (Int, Int)
	-> [a]
	-> GL (Buffer a)
glLoadList usage ix xs = do
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


-- ** Updating Mutable Buffers


unsafeObtainStorableArray :: Buffer a -> IO (StorableArray Int a)
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

glReloadList :: forall a. Storable a =>
	BufferUsage -> (Int, Int) -> [a] -> Buffer a -> GL ()
glReloadList usage ix xs buf@(Buffer _ _ ref) = do
	array <- newListArray ix xs
	bindBuffer array_buffer buf
	withStorableArraySize array $ \size ptr -> do
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
		--bufferData array_buffer usage 0 nullPtr
		bufferData array_buffer usage len ptr


-- ** Buffer Slots

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

