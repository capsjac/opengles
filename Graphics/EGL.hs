-- | http://www.khronos.org/registry/egl/
-- http://www.khronos.org/files/egl-1-4-quick-reference-card.pdf
module Graphics.EGL where
import Control.Applicative
import Data.Word
import Foreign.C.String
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce

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
type EGLint = Int

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
foreign import ccall "EGL/egl.h eglCreatePbufferSurface" c_eglCreatePbufferSurface :: EGLDisplay -> EGLConfig -> Ptr EGLint -> IO EGLSurface
foreign import ccall "EGL/egl.h eglCreatePixmapSurface" c_eglCreatePixmapSurface :: EGLDisplay -> EGLConfig -> EGLNativePixmapType -> Ptr EGLint -> IO EGLSurface
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

type EGL a = IO (Either EGLError a)

-- * Errors
-- 0x3000-0x301E (Reserved 0x300F-0x301F for additional errors)
data EGLError =
  EGLSuccess | EGLNotInitialized | EGLBadAccess | EGLBadAlloc | EGLBadAttribute | EGLBadConfig | EGLBadContext | EGLBadCurrentSurface | EGLBadDisplay | EGLBadMatch | EGLBadNativePixmap | EGLBadNativeWindow | EGLBadParameter | EGLBadSurface | EGLContextLost
  deriving (Enum, Eq)

instance Show EGLError where
  show EGLSuccess = "EGLSuccess: Function succeeded."
  show EGLNotInitialized = "EGLNotInitialized: EGL is not or could not be initialized, for the specified display."
  show EGLBadAccess = "EGLBadAccess: EGL cannot access a requested resource (for example, a context is bound in another thread)."
  show EGLBadAlloc = "EGLBadAlloc: EGL failed to allocate resources for the requested operation."
  show EGLBadAttribute = "EGLBadAttribute: An unrecognized attribute or attribute value was passed in an attribute list."
  show EGLBadConfig = "EGLBadConfig: An EGLConfig argument does not name a valid EGLConfig."
  show EGLBadContext = "EGLBadContext: An EGLContext argument does not name a valid EGLContext."
  show EGLBadCurrentSurface = "EGLBadCurrentSurface: The current surface of the calling thread is a window, pbuffer, or pixmap that is no longer valid."
  show EGLBadDisplay = "EGLBadDisplay: An EGLDisplay argument does not name a valid EGLDisplay."
  show EGLBadMatch = "EGLBadMatch: Arguments are inconsistent; for example, an otherwise valid context requires buffers (e.g. depth or stencil) not allocated by an otherwise valid surface."
  show EGLBadNativePixmap = "EGLBadNativePixmap: An EGLNativePixmapType argument does not refer to a valid native pixmap."
  show EGLBadNativeWindow = "EGLBadNativeWindow: An EGLNativeWindowType argument does not refer to a valid native window."
  show EGLBadParameter = "EGLBadParameter: One or more argument values are invalid."
  show EGLBadSurface = "EGLBadSurface: An EGLSurface argument does not name a valid surface (window, pbuffer, or pixmap) configured for rendering."
  show EGLContextLost = "EGLContextLost: A power management event has occurred. The application must destroy all contexts and reinitialise client API state and objects to continue rendering."
  -- show x = "EGL(Unknown)Error: This error (" ++ show (fromEnum x + 0x3000) ++ ") is not defined in EGL 1.4 spec."

eglGetError :: IO EGLError
eglGetError = c_eglGetError >>= return . toEnum . (-) 0x3000

-- * Attribute Lists
-- 0x3020-0x3042 (Reserved 0x3041-0x304F for additional config attributes)
data EGLAttrib =
    EGLBufferSize EGLint
  | EGLAlphaSize EGLint
  | EGLBlueSize EGLint
  | EGLGreenSize EGLint
  | EGLRedSize EGLint
  | EGLDepthSize EGLint
  | EGLStencilSize EGLint
  | EGLConfigCaveat EGLConfigCaveatValue
  | EGLConfigID EGLint
  | EGLLevel EGLint
  | EGLMaxPbufferHeight EGLint
  | EGLMaxPbufferPixels  EGLint
  | EGLMaxPbufferWidth EGLint
  | EGLNativeRenderable EGLBoolean
  | EGLNativeVisualID EGLint
  | EGLNativeVisualType EGLint
  | EGLSamples EGLint
  | EGLSampleBuffers EGLint
  | EGLSurfaceType EGLint
  | EGLTransparentType EGLTransparentTypeValue
  | EGLTransparentBlueValue EGLint
  | EGLTransparentGreenValue EGLint
  | EGLTransparentRedValue EGLint
  | EGLNone
  | EGLBindToTextureRGB EGLBoolean
  | EGLBindToTextureRGBA EGLBoolean
  | EGLMinSwapInterval EGLint
  | EGLMaxSwapInterval EGLint
  | EGLLuminanceSize EGLint
  | EGLAlphaMaskSize EGLint
  | EGLColorBufferType EGLColorBufferTypeValue
  | EGLRenderableType EGLint
  | EGLMatchNativePixmap EGLint
  | EGLConformant EGLint
  deriving (Eq, Show)

-- EGL_CONFIG_CAVEAT value EGL_NONE | 0x3050 | 0x3051
data EGLConfigCaveatValue = EGLNormalConfig | EGLSlowConfig | EGLNonConformantConfig
  deriving (Eq, Show)
-- EGL_TRANSPARENT_TYPE value EGL_NONE | 0x3052
data EGLTransparentTypeValue = EGLTransparentNone | EGLTransparentRGB
  deriving (Eq, Show)
-- EGL_COLOR_BUFFER_TYPE value 0x308E | 0x308F
data EGLColorBufferTypeValue = EGLRGBBuffer | EGLLuminanceBuffer
  deriving (Eq, Show)

-- EGLBoolean -> EGLError
toEglErr api = api >>= \eglbool ->
  if isTrue eglbool then return EGLSuccess else eglGetError
-- falsy -> Left EGLError, good -> Right result
checkErr api cond f = api >>= \result ->
  if cond result then f result
  else Left <$> eglGetError
isTrue = (/= 0)
notNull = (/= nullPtr)

-- * Initialization & Terminating
eglGetDefaultDisplay :: IO EGLDisplay
eglGetDefaultDisplay = c_eglGetDisplay nullPtr

eglGetDisplay :: EGLNativeDisplay a => a -> IO EGLDisplay
eglGetDisplay = c_eglGetDisplay . getNativeDisplay

eglInitialize :: EGLDisplay -> EGL (Int, Int) -- ^ EGL (major, minor) version
eglInitialize display = alloca $ \major -> alloca $ \minor ->
  checkErr (c_eglInitialize display major minor) isTrue $ \_ ->
    Right <$> ((,) <$> peek major <*> peek minor)

eglTerminate :: EGLDisplay -> IO EGLError
eglTerminate display = toEglErr $ c_eglTerminate display

eglVendor display = c_eglQueryString display 0x3053 >>= peekCString
eglVersion display = c_eglQueryString display 0x3054 >>= peekCString
eglExtensions display = words <$> (c_eglQueryString display 0x3055 >>= peekCString)
eglClientAPIs display = words <$> (c_eglQueryString display 0x308D >>= peekCString)

eglQueryString :: EGLDisplay -> Int -> EGL String
eglQueryString display name =
  checkErr (c_eglQueryString display name) notNull (\str -> Right <$> peekCString str)

eglReleaseThread :: IO EGLError
eglReleaseThread = toEglErr c_eglReleaseThread

-- * Configuration Management
eglGetConfigs :: EGLDisplay -> EGL [EGLConfig]
eglGetConfigs display = alloca $ \num_config ->
  checkErr (c_eglGetConfigs display nullPtr 0 num_config) isTrue $ \_ -> do
    n <- peek num_config
    allocaArray n $ \configs ->
      checkErr (c_eglGetConfigs display configs n num_config) isTrue $ \_ ->
        Right <$> peekArray n configs

-- XXX
eglChooseConfig :: EGLDisplay -> [EGLAttrib] -> EGL [EGLConfig]
eglChooseConfig display attrs = withArray (const [0x3038] attrs) $ \attrib_list -> alloca $ \num_config -> do
  success <- c_eglChooseConfig display attrib_list nullPtr 0 num_config
  if success == 0 then Left <$> eglGetError else do
    n <- peek num_config
    allocaArray n $ \configs -> do
      success <- c_eglChooseConfig display attrib_list configs n num_config
      if success == 0 then Left <$> eglGetError else do
        Right <$> peekArray n configs

-- XXX bad api design
--eglGetConfigAttrib :: EGLDisplay -> EGLConfig -> Int -> EGL EGLAttrib
eglGetConfigAttrib display config attribute = alloca $ \value -> do
  success <- c_eglGetConfigAttrib display config attribute value
  if success /= 0 then Right <$> peek value
  else Left <$> eglGetError

-- * Rendering Surfaces
eglCreateWindowSurface :: EGLNativeWindow a => EGLDisplay -> EGLConfig -> a -> [EGLAttrib] -> EGL EGLSurface
eglCreateWindowSurface display config win attrs =
  withArray (const [0x3038] attrs) $ \attrib_list ->
    checkErr (c_eglCreateWindowSurface display config (getNativeWindow win) attrib_list) notNull (return . Right)

eglCreatePbufferSurface :: EGLDisplay -> EGLConfig -> [EGLAttrib] -> EGL EGLSurface
eglCreatePbufferSurface display config attrs =
  withArray (const [0x3038] attrs) $ \attrib_list ->
    checkErr (c_eglCreatePbufferSurface display config attrib_list) notNull (return . Right)

--eglCreatePbufferFromClientBuffer :: EGLDisplay -> EGLenum -> EGLClientBuffer -> EGLConfig -> [EGLAttrib] -> IO EGLSurface
eglDestroySurface :: EGLDisplay -> EGLSurface -> IO EGLError
eglDestroySurface display surface = toEglErr (c_eglDestroySurface display surface)

eglCreatePixmapSurface :: EGLNativePixmap a => EGLDisplay -> EGLConfig -> a ->  [EGLAttrib] -> EGL EGLSurface
eglCreatePixmapSurface display config pixmap attrs =
  withArray (const [0x3038] attrs) $ \attrib_list ->
    checkErr (c_eglCreatePixmapSurface display config (getNativePixmap pixmap) attrib_list) notNull (return . Right)

--eglSurfaceAttrib :: EGLDisplay -> EGLSurface -> EGLint -> EGLint -> IO EGLError
--eglQuerySurface :: EGLDisplay -> EGLSurface -> EGLint -> EGL EGLint

-- * Rendering Contexts
data EGLBindAPIValue = EGLOpenGLAPI | EGLOpenGLESAPI | EGLOpenVGAPI | EGLAPINone
  deriving (Eq, Show)
--eglBindAPI :: EGLBindAPIValue -> IO Bool
--eglQueryAPI :: IO EGLBindAPIValue
--eglCreateContext :: EGLDisplay -> EGLConfig -> EGLContext -> [EGLint] -> EGL EGLContext
--eglDestroyContext :: EGLDisplay -> EGLContext -> IO EGLError

eglMakeCurrent :: EGLDisplay -> EGLSurface -> EGLSurface -> EGLContext -> IO EGLError
eglMakeCurrent display draw read context =
  toEglErr (c_eglMakeCurrent display draw read context)

eglGetCurrentContext :: EGL EGLContext
eglGetCurrentContext =
  checkErr c_eglGetCurrentContext notNull (return . Right)

eglGetCurrentSurface :: EGLint -> EGL EGLSurface
eglGetCurrentSurface readdraw =
  checkErr (c_eglGetCurrentSurface readdraw) notNull (return . Right)

eglGetCurrentDisplay :: EGL EGLDisplay
eglGetCurrentDisplay =
  checkErr c_eglGetCurrentDisplay notNull (return . Right)

eglQueryContext :: EGLDisplay -> EGLContext -> EGLint -> EGL EGLint
eglQueryContext display context attribute = alloca $ \value ->
  checkErr (c_eglQueryContext display context attribute value) isTrue (\_ -> Right <$> peek value)

-- * Synchronization Primitives
eglWaitClient :: IO EGLError
eglWaitClient = toEglErr c_eglWaitClient
-- eglWaitGL is available for backwards compatibility
-- eglWaitGL :: IO EGLError
-- eglWaitGL = toEglErr c_eglWaitGL

eglWaitNative :: EGLint -> IO EGLError
eglWaitNative engine = toEglErr (c_eglWaitNative engine)

-- * Posting the Color Buffer
eglSwapBuffers :: EGLDisplay -> EGLSurface -> IO EGLError
eglSwapBuffers display surface =
  toEglErr (c_eglSwapBuffers display surface)

eglCopyBuffers :: EGLNativePixmap a => EGLDisplay -> EGLSurface -> a -> IO EGLError
eglCopyBuffers display surface pixmap =
  toEglErr (c_eglCopyBuffers display surface (getNativePixmap pixmap))

eglSwapInterval :: EGLDisplay -> Int -> IO EGLError
eglSwapInterval display interval =
  toEglErr (c_eglSwapInterval display (unsafeCoerce interval))

-- * Render to Textures
eglBindTexImage :: EGLDisplay -> EGLSurface -> EGLint -> IO EGLError
eglBindTexImage display surface buffer =
  toEglErr (c_eglBindTexImage display surface (buffer))

eglReleaseTexImage :: EGLDisplay -> EGLSurface -> EGLint -> IO EGLError
eglReleaseTexImage display surface buffer =
  toEglErr (c_eglReleaseTexImage display surface (buffer))

-- * Obtain Extension Function Pointers
eglGetProcAddress :: String -> FunPtr a
eglGetProcAddress procname =
  unsafePerformIO $ withCString procname (unsafeCoerce . c_eglGetProcAddress)

-- * Extending EGL
