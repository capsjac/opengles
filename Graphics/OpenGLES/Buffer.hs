{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Graphics.OpenGLES.Buffer (

  -- * Buffer
  -- ** Constructing Mutable Buffers
  Buffer,
  GLArray,
  GLSource(..),
  glLoad, glReload, glUnsafeRead, glModify, glMap,
  -- ** Updating Mutable Buffers
  withStorableArraySize,
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
  -- | /ES 3+/
  pixel_pack_buffer, pixel_unpack_buffer,
  uniform_buffer, transform_feedback_buffer,
  -- | /3+ or GL_NV_copy_buffer/
  copy_read_buffer, copy_write_buffer
  ) where

import Control.Applicative
import Data.Array.Storable
import Data.Array.Storable.Internals
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.IORef
import Graphics.OpenGLES.Base
import Graphics.OpenGLES.Caps
import Graphics.OpenGLES.Internal
import Graphics.OpenGLES.Types
import Foreign hiding (newArray)

-- ** Constructing Mutable Buffers

type GLArray a = StorableArray Int a

type family Content a x :: *
type instance Content Int x = x
type instance Content B.ByteString x = x
type instance Content [a] x = a
type instance Content ([a], Int) x = a
type instance Content (GLArray a) x = a

class (Storable b, b ~ Content a b) => GLSource a b where
	makeAref :: a -> GL (Either (GLArray (Content a b)) Int)
	makeWriter :: a -> (Ptr (Content a b) -> GL (), Int)

instance Storable b => GLSource Int b where
	makeAref = return . Right
	makeWriter len = (\ptr -> B.memset (castPtr ptr) 0
		(fromIntegral $ len * sizeOf (undefined :: b))
			>> return (), len)

instance Storable a => GLSource ([a], Int) a where
	makeAref (xs, len) = Left <$> newListArray (0, len - 1) (cycle xs)
	makeWriter (xs, len) = (\ptr -> pokeArray ptr xs, len)

instance Storable a => GLSource [a] a where
	makeAref xs = Left <$> newListArray (0, length xs - 1) xs
	makeWriter xs = (\ptr -> pokeArray ptr xs, length xs)

instance Storable b => GLSource B.ByteString b where
	makeAref bs@(B.PS foreignPtr offset len) = do
		let fp | offset == 0 = foreignPtr
		       | otherwise = case B.copy bs of (B.PS f _ _) -> f
		let elems = (len `div` sizeOf (undefined :: b))
		let array = StorableArray 0 (elems-1) elems (castForeignPtr fp)
		return (Left array)
	makeWriter (B.PS fp offset len) = (\dst ->
		withForeignPtr fp $ \src ->
			B.memcpy (castPtr dst) (advancePtr (castPtr src) offset) len
		, len `div` sizeOf (undefined :: b))

instance Storable a => GLSource (GLArray a) a where
	makeAref = return . Left
	makeWriter sa@(StorableArray _ _ len fp) =
		(\dst -> withForeignPtr fp $ \src ->
			B.memcpy (castPtr dst) (castPtr src)
			(len * sizeOf (undefined :: a))
		, len)

--instance GLSource (Buffer a) a where
--	makeAref (Buffer aref glo) = return . Left =<< go =<< readIORef aref
--	where
--		go (Left)
--		go (Right) 


-- |
-- Create and initialize a 'Buffer' storage on GPU working memory.
-- 
-- - Int → /O(1)/
-- New Buffer with specified number of elements initialized to zeros.
-- - ([a], Int) → /O(n)/
-- New Buffer made of given list upto n th element.
-- - [a] → /O(n)/
-- New Buffer made of given list. Same as (xs, length xs)
-- - 'ByteString' → /head O(1) otherwise O(n)/
-- New Buffer from 'ByteString'. Might be copied when necessary.
-- - 'GLArray' → __Unsafe__ /O(1)/
-- Use passed 'StorableArray' as client-side Buffer.
-- 
-- > glLoad app2gl (10::Int) :: GL (Buffer Vec4)
-- > glLoad app2gl ([V2 1 1],4::Int) :: GL (Buffer Vec2)
-- > glLoad app2gl uv :: GL (Buffer (V2 Word8))
-- > glLoad app2gl bs :: GL (Buffer Float)
-- > glLoad app2gl (model :: GLArray Model) :: GL (Buffer Model)
glLoad :: forall a b. GLSource a b => BufferUsage -> a -> GL (Buffer b)
glLoad usage src = do
	aref <- newIORef =<< makeAref src
	Buffer aref <$> newBuffer (do
		array <- readIORef aref
		case array of
			Left sa ->
				withStorableArraySize sa (bufferData array_buffer usage)
			Right elems ->
				bufferData array_buffer usage (elems * unit) nullPtr
		void $ showError "glBufferData"
		) where unit = sizeOf (undefined :: b)
	-- TODO BufferArchive

newBuffer init = newGLO glGenBuffers glDeleteBuffers
	(\i -> glBindBuffer 0x8892 i >> init) -- GL_ARRAY_BUFFER

newArrayLen elems unit = do
	sa@(StorableArray _ _ _ fp) <- newArray_ (0, elems - 1)
	withForeignPtr fp $ \dst ->
		B.memset (castPtr dst) 0 (fromIntegral $ elems * unit)
	return sa

glReload :: forall a b. GLSource a b => Buffer b -> Int -> a -> GL ()
glReload buf@(Buffer aref glo) offsetIx src = do
	bindBuffer array_buffer buf
	aref' <- readIORef aref
	let unit = sizeOf (undefined :: b)
	let (fillSubArray, size') = makeWriter src
	let size = size' * unit
	if hasES3 then do
		ptr <- mapBufferRange array_buffer (offsetIx * unit) size
			(map_write_bit + map_invalidate_range_bit + map_unsynchronized_bit)
		showError "glMapBufferRange"
		fillSubArray ptr
		unmapBuffer array_buffer
		showError "glUnmapBuffer"
		case aref' of Left (StorableArray _ _ len _) ->
				writeIORef aref (Right (len * unit))
	else do
		sa@(StorableArray _ _ len fp) <- case aref' of
			Left array -> return array
			Right elems -> newArrayLen elems unit
		withForeignPtr fp $ \p -> do
			let ptr = advancePtr p (offsetIx * unit)
			fillSubArray ptr
			bufferSubData array_buffer (offsetIx * unit) size ptr
			showError "glBufferSubData"
		writeIORef aref (Left sa)


glUnsafeRead :: forall a. Storable a => Buffer a -> (Int, Int) -> GL (GLArray a)
glUnsafeRead buf@(Buffer aref glo) (offsetIx, len) = do
	bindBuffer array_buffer buf
	array <- readIORef aref
	case array of
		Left (StorableArray s e l fp) -> -- unsafe!
			return (StorableArray s e l fp)
		Right elems -> do
			sa <- newArrayLen (min len (elems - offsetIx)) unit
			if hasES3 then do
				src <- mapBufferRange array_buffer (offsetIx * unit) (len * unit)
					(map_read_bit {- + map_unsynchronized_bit-})
				withStorableArray sa $ \dst ->
					B.memcpy (castPtr dst) src (len * unit)
				unmapBuffer array_buffer
				writeIORef aref (Left sa) -- backup
				return sa
			else return sa
	where unit = sizeOf (undefined :: a)

glModify :: forall a. Storable a => Buffer a -> (Int, Int) -> (GLArray a -> GL ()) -> GL ()
glModify buf@(Buffer aref glo) (offsetIx, len) f = do
	bindBuffer array_buffer buf
	if hasES3 then do
		a <- readIORef aref
		let elems = case a of
			Right elems -> elems
			Left (StorableArray _ _ elems _) -> elems 
		ptr <- mapBufferRange array_buffer 0 (len * unit)
					(map_read_bit + map_write_bit {- + map_unsynchronized_bit-})
		fp <- newForeignPtr_ ptr
		f $ StorableArray 0 (elems-1) elems fp
		unmapBuffer array_buffer
		writeIORef aref (Right elems)
	else do
		a <- readIORef aref
		case a of 
			Left sa -> do
				f sa
				withStorableArraySize sa (bufferSubData array_buffer 0)
			Right elems -> do
				sa <- newArrayLen elems unit
				f sa
				withStorableArraySize sa (bufferSubData array_buffer 0)
	where unit = sizeOf (undefined :: a)

glMap :: Storable a => (a -> GL a) -> Buffer a -> (Int, Int) -> GL ()
glMap f buffer offLen = glModify buffer offLen $
	\(StorableArray _ _ len fp) ->
 	withForeignPtr fp $ \ptr ->
		sequence_
			[ peekElemOff ptr i >>= f >>= pokeElemOff ptr i
			| i <- [0..len-1] ]


-- ** Updating Mutable Buffers


withStorableArraySize
	:: forall i e a. Storable e
	=> StorableArray i e -> (Int -> Ptr e -> IO a) -> IO a
withStorableArraySize (StorableArray _ _ n fp) f =
	withForeignPtr fp (f size)
	where size = n * sizeOf (undefined :: e)

-- hasMapBufferRange = hasES3
-- GL_NV_map_buffer_range	5devices	2%
-- GL_EXT_map_buffer_range	2devices	1%

-- Performance hint http://www.opentk.com/node/1930


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
bindBuffer (BufferSlot target) (Buffer _ glo) =
	glBindBuffer target =<< getObjId glo

bindBufferRange :: BufferSlot -> GLuint -> Buffer a -> Int -> Int -> GL ()
bindBufferRange (BufferSlot t) index (Buffer _ glo) offset size = do
	buf <- getObjId glo
	glBindBufferRange t index buf offset size

bindBufferBase :: BufferSlot -> GLuint -> Buffer a -> GL ()
bindBufferBase (BufferSlot t) index (Buffer _ glo) = do
	glBindBufferBase t index =<< getObjId glo

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

