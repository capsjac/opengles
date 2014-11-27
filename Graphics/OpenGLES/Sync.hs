module Graphics.OpenGLES.Sync where

import Control.Applicative
import Control.Monad
import Data.IORef
import Foreign
import Graphics.OpenGLES.Base
import Graphics.OpenGLES.Internal


data Sync = Sync Word64 (IORef GLsync)

-- | Obtain new 'Sync' with or without timeout in nanoseconds.
-- Fence sync objects are used to wait for partial completion of the GL
-- command stream, as a more flexible form of glFinish.
-- 
-- > -- Sync objects can be used many times
-- > sync1 <- getSync (Just 16000)
-- > sync2 <- getSync Nothing
-- 
-- For each frame:
-- 
-- > glFence sync1 $ \isTimedOut -> do
-- >   {- modify buffers, textures, etc -}
-- > glFence sync2 $ \isTimedOut -> do
-- >   {- modify buffers, textures, etc -}
-- > endFrameGL
getSync :: Maybe Word64 -> IO Sync
getSync timeout_ns = Sync timeout <$> newIORef nullPtr
	where
		timeout = maybe timeoutIgnored id timeout_ns
		-- GL_TIMEOUT_IGNORED
		timeoutIgnored = 0xFFFFFFFFFFFFFFFF

-- | Block and wait for GPU commands issued here complete.
-- Better glFinish for /ES 3+/.
-- Block and wait for a 'Sync' object to become signaled, then run specified block.
glFence :: Sync -> (Bool -> GL a) -> GL a
glFence sync io = do
	isExpired <- waitFence False sync 
	result <- io isExpired
	createFence sync
	return result

-- | Blocks on GPU until GL commands issued here complete.
-- Better glFlush for /ES 3+/. Sync timeout is ignored.
-- Instruct the GL server to block (on the GPU) until the previous call of
-- glFence* with specified 'Sync' object becomes finished on the GL server,
-- then run specified block.
glFenceInGpu :: Sync -> GL a -> GL a
glFenceInGpu sync io = do
	waitFenceAtGpu sync
	result <- io
	createFence sync
	return result

waitFence :: Bool -> Sync -> GL Bool
waitFence flushCmdQ (Sync timeout ref) = do
	sync <- readIORef ref
	if sync /= nullPtr then do
		result <- glClientWaitSync sync flushBit timeout
		glDeleteSync sync
		writeIORef ref nullPtr
		return $ case result of
			-- GL_ALREADY_SIGNALED
			0x911A -> False -- error "glFence: AlreadySignaled"
			-- GL_CONDITION_SATISFIED
			0x911C -> False
			-- GL_TIMEOUT_EXPIRED
			0x911B -> True
			-- GL_WAIT_FAILED
			0x911D -> error "glFence: WaitFailed"
			-- GL_INVALID_VALUE
			_ {-0x0501-} -> error "glFence: InvalidValue"
	else return False
	where
		-- GL_SYNC_FLUSH_COMMANDS_BIT
		flushBit = if flushCmdQ then 1 else 0

createFence :: Sync -> GL ()
createFence (Sync _ ref) = do
	-- GL_SYNC_GPU_COMMANDS_COMPLETE
	sync <- glFenceSync 0x9117 0
	writeIORef ref sync

waitFenceAtGpu :: Sync -> GL ()
waitFenceAtGpu (Sync _ ref) = do
	sync <- readIORef ref
	when (sync /= nullPtr) $ do
		result <- glWaitSync sync 0 0xFFFFFFFFFFFFFFFF
		glDeleteSync sync
		writeIORef ref nullPtr

-- | Same as glFlush
glFlushCommandQ :: GL ()
glFlushCommandQ = glFlush

-- | Same as glFinish
glWaitComplete :: GL ()
glWaitComplete = glFinish

