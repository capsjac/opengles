{-# LANGUAGE CPP, Unsafe, UnliftedFFITypes, MagicHash, UnboxedTuples #-}
module Graphics.OpenGLES.Base.Proc (glGetProcAddress) where
import Foreign
import Foreign.C.String
import GHC.Base (realWorld#)
import GHC.CString (unpackCString#)
import GHC.IO (IO (IO))
import GHC.Ptr (Ptr(..))
import System.IO.Unsafe (unsafePerformIO)


#if defined(EGL_GETPROC)
foreign import ccall unsafe "eglGetProcAddress"
	getProcAddress :: CString -> IO (FunPtr a)

#elif defined(WGL_GETPROC)
foreign import ccall unsafe "wglGetProcAddress"
	getProcAddress :: CString -> IO (FunPtr a)

#elif defined(GLX_GETPROC)
foreign import ccall unsafe "glXGetProcAddress"
	getProcAddress :: CString -> IO (FunPtr a)

#elif defined(DLSYM_GETPROC)
-- | void *dlsym(void *handle, const char *symbol);
foreign import ccall unsafe "dlfcn.h dlsym"
	dlsym :: Ptr () -> CString -> IO (FunPtr a)

getProcAddress :: CString -> IO (FunPtr a)
getProcAddress procname = dlsym runtime_loader_default procname
	where runtime_loader_default = nullPtr
#else

#error "Don't know how to retrieve OpenGL ES extension entries. try -DEGL_GETPROC"

#endif


-- * Retrieve OpenGL (ES) and EGL Extension Entries

-- | Just like unsafePerformIO, but we inline it. Big performance gains as
-- it exposes lots of things to further inlining. /Very unsafe/. In
-- particular, you should do no memory allocation inside an
-- 'inlinePerformIO' block.
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
{-# INLINE inlinePerformIO #-}

glGetProcAddress :: String -> FunPtr a
glGetProcAddress procname =
	unsafePerformIO $ withCString procname getProcAddress
{-# INLINE [0] glGetProcAddress #-}
{-# RULES "glGetProcAddress/getProcAddress"
	forall s. glGetProcAddress (unpackCString# s) =
		inlinePerformIO (getProcAddress (Ptr s))
	#-}

