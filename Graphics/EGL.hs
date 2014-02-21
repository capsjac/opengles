-- | http://www.khronos.org/registry/egl/
module Graphics.EGL where
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

-- * Types
-- from EGL/eglplatform.h
-- ** Window-system-dependent types
class EGLNativeWindowType a where getNativeWindowPtr :: a -> Ptr ()
class EGLNativePixmapType a where getNativePixmapPtr :: a -> Ptr ()
-- XXX Symbian platform uses int instead of pointer...
class EGLNativeDisplayType a where getNativeDisplayPtr :: a -> Ptr ()

-- ** EGL Types
-- | 32-bit signed integer
type EGLint = Int32

-- from EGL/egl.h
type EGLBoolean = Word32
type EGLenum = Word32
type EGLConfig = Ptr ()
type EGLContext = Ptr ()
type EGLDisplay = Ptr ()
type EGLSurface = Ptr ()
type EGLClientBuffer = Ptr ()

-- * EGL Functions
foreign import ccall "EGL/egl.h eglGetError" c_eglGetError :: IO EGLint

foreign import ccall "EGL/egl.h eglGetDisplay" c_eglGetDisplay :: EGLNativeDisplayType -> IO EGLDisplay
foreign import ccall "EGL/egl.h eglInitialize" c_eglInitialize :: EGLDisplay -> Ptr EGLint -> Ptr EGLint -> IO EGLBoolean
foreign import ccall "EGL/egl.h eglTerminate" c_eglTerminate :: EGLDisplay -> IO EGLBoolean

foreign import ccall "EGL/egl.h eglQueryString" c_eglQueryString :: EGLDisplay -> EGLint -> IO CString

foreign import ccall "EGL/egl.h eglChooseConfig" c_eglChooseConfig :: EGLDisplay -> Ptr EGLint -> Ptr EGLConfig -> EGLint -> Ptr EGLint -> IO EGLBoolean
foreign import ccall "EGL/egl.h eglGetConfigAttrib" c_eglGetConfigAttrib :: EGLDisplay -> EGLConfig -> EGLint -> Ptr EGLint -> IO EGLBoolean

foreign import ccall "EGL/egl.h eglCreateWindowSurface" c_eglCreateWindowSurface :: EGLDisplay -> EGLConfig -> EGLNativeWindowType -> Ptr EGLint -> IO EGLSurface
foreign import ccall "EGL/egl.h eglDestroySurface" c_eglDestroySurface :: EGLDisplay -> EGLSurface -> IO Word32
foreign import ccall "EGL/egl.h eglQuerySurface" c_eglQuerySurface :: EGLDisplay -> EGLSurface -> EGLint -> Ptr EGLint -> IO EGLBoolean

foreign import ccall "EGL/egl.h eglSwapInterval" c_eglSwapInterval :: EGLDisplay -> EGLint -> IO EGLBoolean

foreign import ccall "EGL/egl.h eglCreateContext" c_eglCreateContext :: EGLDisplay -> EGLConfig -> EGLContext -> Ptr EGLint -> IO EGLContext
foreign import ccall "EGL/egl.h eglDestroyContext" c_eglDestroyContext :: EGLDisplay -> EGLContext -> IO Word32
foreign import ccall "EGL/egl.h eglMakeCurrent" c_eglMakeCurrent :: EGLDisplay -> EGLSurface -> EGLSurface -> EGLContext -> IO Word32

foreign import ccall "EGL/egl.h eglSwapBuffers" c_eglSwapBuffers :: EGLDisplay -> EGLSurface -> IO Word32
