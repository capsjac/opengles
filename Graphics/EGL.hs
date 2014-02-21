-- | http://www.khronos.org/registry/egl/
module Graphics.EGL where
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

-- * Types
-- from EGL/eglplatform.h
-- ** Window-system-dependent types
type EGLNativeWindowType = Ptr ()
type EGLNativePixmapType = Ptr ()
-- XXX Symbian platform uses int instead of pointer...
type EGLNativeDisplayType = Ptr ()
class EGLNativeWindow a where getNativeWindow :: a -> EGLNativeWindowType
class EGLNativePixmap a where getNativePixmap :: a -> EGLNativePixmapType
class EGLNativeDisplay a where getNativeDisplay :: a -> EGLNativeDisplayType

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

foreign import ccall "EGL/egl.h eglGetConfigs" c_eglGetConfigs :: EGLDisplay -> Ptr EGLConfig -> EGLint -> Ptr EGLint -> IO EGLBoolean
foreign import ccall "EGL/egl.h eglChooseConfig" c_eglChooseConfig :: EGLDisplay -> Ptr EGLint -> Ptr EGLConfig -> EGLint -> Ptr EGLint -> IO EGLBoolean
foreign import ccall "EGL/egl.h eglGetConfigAttrib" c_eglGetConfigAttrib :: EGLDisplay -> EGLConfig -> EGLint -> Ptr EGLint -> IO EGLBoolean

foreign import ccall "EGL/egl.h eglCreateWindowSurface" c_eglCreateWindowSurface :: EGLDisplay -> EGLConfig -> EGLNativeWindowType -> Ptr EGLint -> IO EGLSurface
foreign import ccall "EGL/egl.h eglCreatePbufferSurface" c_eglCreateWindowSurface :: EGLDisplay -> EGLConfig -> Ptr EGLint -> IO EGLSurface
foreign import ccall "EGL/egl.h eglCreatePixmapSurface" c_eglCreateWindowSurface :: EGLDisplay -> EGLConfig -> EGLNativePixmapType -> Ptr EGLint -> IO EGLSurface
foreign import ccall "EGL/egl.h eglDestroySurface" c_eglDestroySurface :: EGLDisplay -> EGLSurface -> IO EGLBoolean
foreign import ccall "EGL/egl.h eglQuerySurface" c_eglQuerySurface :: EGLDisplay -> EGLSurface -> EGLint -> Ptr EGLint -> IO EGLBoolean

foreign import ccall "EGL/egl.h eglBindAPI" c_eglBindAPI :: EGLenum -> IO EGLBoolean
foreign import ccall "EGL/egl.h eglQueryAPI" c_eglQueryAPI :: IO EGLenum

foreign import ccall "EGL/egl.h eglWaitClient" c_eglWaitClient :: IO EGLBoolean

foreign import ccall "EGL/egl.h eglReleaseThread" c_eglReleaseThread :: IO EGLBoolean

foreign import ccall "EGL/egl.h eglCreatePbufferFromClientBuffer" c_eglCreatePbufferFromClientBuffer :: EGLDisplay -> EGLenum -> EGLClientBuffer -> EGLConfig -> Ptr EGLint -> IO EGLSurface

foreign import ccall "EGL/egl.h eglSurfaceAttrib" c_eglSurfaceAttrib :: EGLDisplay -> EGLSurface -> EGLint -> EGLint -> IO EGLBoolean
foreign import ccall "EGL/egl.h eglBindTexImage" c_eglBindTexImage :: EGLDisplay -> EGLSurface -> EGLint -> IO EGLBoolean
foreign import ccall "EGL/egl.h eglReleaseTexImage" c_eglReleaseTexImage :: EGLDisplay -> EGLSurface -> EGLint -> IO EGLBoolean

foreign import ccall "EGL/egl.h eglSwapInterval" c_eglSwapInterval :: EGLDisplay -> EGLint -> IO EGLBoolean

foreign import ccall "EGL/egl.h eglCreateContext" c_eglCreateContext :: EGLDisplay -> EGLConfig -> EGLContext -> Ptr EGLint -> IO EGLContext
foreign import ccall "EGL/egl.h eglDestroyContext" c_eglDestroyContext :: EGLDisplay -> EGLContext -> IO EGLBoolean
foreign import ccall "EGL/egl.h eglMakeCurrent" c_eglMakeCurrent :: EGLDisplay -> EGLSurface -> EGLSurface -> EGLContext -> IO EGLBoolean

foreign import ccall "EGL/egl.h eglGetCurrentContext" c_eglGetCurrentContext :: IO EGLContext
foreign import ccall "EGL/egl.h eglGetCurrentSurface" c_eglGetCurrentSurface :: EGLint -> IO EGLSurface
foreign import ccall "EGL/egl.h eglGetCurrentDisplay" c_eglGetCurrentDisplay :: IO EGLDisplay
foreign import ccall "EGL/egl.h eglQueryContext" c_eglQueryContext :: EGLDisplay -> EGLContext -> EGLint -> Ptr EGLint -> IO EGLBoolean

foreign import ccall "EGL/egl.h eglWaitGL" c_eglWaitGL :: IO EGLBoolean
foreign import ccall "EGL/egl.h eglWaitNative" c_eglWaitNative :: EGLint -> IO EGLBoolean
foreign import ccall "EGL/egl.h eglSwapBuffers" c_eglSwapBuffers :: EGLDisplay -> EGLSurface -> IO EGLBoolean
foreign import ccall "EGL/egl.h eglCopyBuffers" c_eglCopyBuffers :: EGLDisplay -> EGLSurface -> EGLNativePixmapType -> IO EGLBoolean

foreign import ccall "EGL/egl.h eglGetProcAddress" c_eglGetProcAddress :: CString -> IO (FunPtr  ())
