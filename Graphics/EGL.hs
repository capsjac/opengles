{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__
{-# LANGUAGE Unsafe, UnliftedFFITypes, MagicHash, UnboxedTuples #-}
#endif
-- | EGL 1.4 standard
-- 
-- <http://www.khronos.org/registry/egl/>
-- 
-- <http://www.khronos.org/files/egl-1-4-quick-reference-card.pdf>
module Graphics.EGL where
import Control.Applicative
import Foreign
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)
#if __GLASGOW_HASKELL__
import GHC.Base (realWorld#)
import GHC.CString (unpackCString#)
import GHC.IO (IO (IO))
import GHC.Ptr (Ptr(..))
#else
import System.IO.Unsafe
#endif

type EGL a = IO (Either EGLError a)

-- * Errors
-- 0x3000-0x301E (Reserved 0x300F-0x301F for additional errors)
data EGLError =
    EGLSuccess
  | EGLNotInitialized
  | EGLBadAccess
  | EGLBadAlloc
  | EGLBadAttribute
  | EGLBadConfig
  | EGLBadContext
  | EGLBadCurrentSurface
  | EGLBadDisplay
  | EGLBadMatch
  | EGLBadNativePixmap
  | EGLBadNativeWindow
  | EGLBadParameter
  | EGLBadSurface
  | EGLContextLost
  | EGLUnknownErr Int

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
  show (EGLUnknownErr n) = "EGLUnknownError: This error (" ++ show n ++ ") is not defined in EGL 1.4 spec."

eglGetError :: IO EGLError
eglGetError = c_eglGetError >>= return . unMarshal . (-) 0x3000
  where unMarshal x | x < 15 =
          [EGLSuccess
          ,EGLNotInitialized
          ,EGLBadAccess
          ,EGLBadAlloc
          ,EGLBadAttribute
          ,EGLBadConfig
          ,EGLBadContext
          ,EGLBadCurrentSurface
          ,EGLBadDisplay
          ,EGLBadMatch
          ,EGLBadNativePixmap
          ,EGLBadNativeWindow
          ,EGLBadParameter
          ,EGLBadSurface
          ,EGLContextLost
          ] !! x
        unMarshal x = EGLUnknownErr x

-- * Attribute Lists
-- 0x3020-0x3042 (Reserved 0x3041-0x304F for additional config attributes)
data EGLConfAttr =
    EGLBufferSize --EGLint
  | EGLAlphaSize --EGLint
  | EGLBlueSize --EGLint
  | EGLGreenSize --EGLint
  | EGLRedSize --EGLint
  | EGLDepthSize --EGLint
  | EGLStencilSize --EGLint
  | EGLConfigCaveat --EGLConfigCaveatValue
  | EGLConfigID --EGLint
  | EGLLevel --EGLint
  | EGLMaxPbufferHeight --EGLint
  | EGLMaxPbufferPixels --EGLint
  | EGLMaxPbufferWidth --EGLint
  | EGLNativeRenderable --EGLBoolean
  | EGLNativeVisualID --EGLint
  | EGLNativeVisualType --EGLint
  | EGLSamples --EGLint
  | EGLSampleBuffers --EGLint
  | EGLSurfaceType --EGLint
  | EGLTransparentType --EGLTransparentTypeValue
  | EGLTransparentBlueValue --EGLint
  | EGLTransparentGreenValue --EGLint
  | EGLTransparentRedValue --EGLint
  | EGLBindToTextureRGB --EGLBoolean
  | EGLBindToTextureRGBA --EGLBoolean
  | EGLMinSwapInterval --EGLint
  | EGLMaxSwapInterval --EGLint
  | EGLLuminanceSize --EGLint
  | EGLAlphaMaskSize --EGLint
  | EGLColorBufferType --EGLColorBufferTypeValue
  | EGLRenderableType --EGLint
  | EGLMatchNativePixmap -- ^ EGLint Pseudo-attribute (not queryable)
  | EGLConformant --EGLint

-- XXX deprecated instance
instance Enum EGLConfAttr where
  fromEnum x = case x of
    EGLBufferSize -> 0x3020
    EGLAlphaSize -> 0x3021
    EGLBlueSize -> 0x3022
    EGLGreenSize -> 0x3023
    EGLRedSize -> 0x3024
    EGLDepthSize -> 0x3025
    EGLStencilSize -> 0x3026
    EGLConfigCaveat -> 0x3027
    EGLConfigID -> 0x3028
    EGLLevel -> 0x3029
    EGLMaxPbufferHeight -> 0x302A
    EGLMaxPbufferPixels -> 0x302B
    EGLMaxPbufferWidth -> 0x302C
    EGLNativeRenderable -> 0x302D
    EGLNativeVisualID -> 0x302E
    EGLNativeVisualType -> 0x302F
    EGLSamples -> 0x3031
    EGLSampleBuffers -> 0x3032
    EGLSurfaceType -> 0x3033
    EGLTransparentType -> 0x3034
    EGLTransparentBlueValue -> 0x3035
    EGLTransparentGreenValue -> 0x3036
    EGLTransparentRedValue -> 0x3037
    EGLBindToTextureRGB -> 0x3039
    EGLBindToTextureRGBA -> 0x303A
    EGLMinSwapInterval -> 0x303B
    EGLMaxSwapInterval -> 0x303C
    EGLLuminanceSize -> 0x303D
    EGLAlphaMaskSize -> 0x303E
    EGLColorBufferType -> 0x303F
    EGLRenderableType -> 0x3040
    EGLMatchNativePixmap -> 0x3041
    EGLConformant -> 0x3042

egl_true = 1 :: Int
egl_false = 0 :: Int
-- | EGL_CONFIG_CAVEAT value EGL_NONE | 0x3050 | 0x3051
egl_none = 0x3038 :: Int
egl_slow_config = 0x3050 :: Int
egl_non_conformant_config = 0x3051 :: Int

-- | EGL_TRANSPARENT_TYPE value EGL_NONE | 0x3052
egl_transparent_rgb = 0x3052 :: Int

-- | EGL_COLOR_BUFFER_TYPE value 0x308E | 0x308F
egl_rgb_buffer = 0x308E :: Int
egl_luminance_buffer = 0x308F :: Int

-- | EGL_RENDERABLE_TYPE bitmask
egl_opengl_es_bit = 1 :: Int
egl_openvg_bit = 2 :: Int
egl_opengl_es2_bit = 4 :: Int
egl_opengl_bit = 8 :: Int

data EGLSurfAttr =
      EGLVGAlphaFormat -- enum Alpha format for OpenVG
    | EGLVGColorspace -- enum Color space for OpenVG
    | EGLConfigId -- Integer ID of EGLConfig surface was created with EGL_HEIGHT integer Height of surface
    | EGLHeight -- Height of surface
    | EGLHorizontalResolution -- integer Horizontal dot pitch
    | EGLLargestPbuffer -- boolean If true, create largest pbuffer possible
    | EGLMipmapTexture -- boolean True if texture has mipmaps
    | EGLMipmapLevel -- integer Mipmap level to render to
    | EGLMultisampleResolve -- enum Multisample resolve behavior
    | EGLPixelAspectRaito -- integer Display aspect ratio
    | EGLRenderBuffer -- enum Render buffer
    | EGLSwapBehavior -- enum Buffer swap behavior
    | EGLTextureFormat -- enum Format of texture: RGB, RGBA, or no texture
    | EGLTextureTarget -- enum Type of texture: 2D or no texture
    | EGLVerticalResolution -- integer Vertical dot pitch
    | EGLWidth -- integer Width of surface

instance Enum EGLSurfAttr where
  fromEnum x = case x of
    EGLHeight -> 0x3056
    EGLWidth -> 0x3067
    EGLLargestPbuffer -> 0x3058
    EGLTextureFormat -> 0x3080
    EGLTextureTarget -> 0x3081
    EGLMipmapTexture -> 0x3082
    EGLMipmapLevel -> 0x3083
    EGLRenderBuffer -> 0x3086
    EGLVGColorspace -> 0x3087
    EGLVGAlphaFormat -> 0x3088
    EGLHorizontalResolution -> 0x3090
    EGLVerticalResolution -> 0x3091
    EGLPixelAspectRaito -> 0x3092
    EGLSwapBehavior -> 0x3093
    EGLMultisampleResolve -> 0x3099
    EGLConfigId -> 0x3028

-- EGLBoolean -> EGLError
toEglErr api = api >>= \eglbool ->
  if isTrue eglbool then return EGLSuccess else eglGetError
-- falsy -> Left EGLError, good -> Right result
checkErr api cond f = api >>= \result ->
  if cond result then f result else Left <$> eglGetError
checkPtr api wrap = api >>= \ptr ->
  if ptr /= nullPtr then return (Right $ wrap ptr) else Left <$> eglGetError
checkBool api f = api >>= \bool ->
  if isTrue bool then Right <$> f else Left <$> eglGetError
isTrue = (/= 0)
withAttrList attrs =
  withArray $ foldr (\(k, v) l -> fromEnum k : v : l) [0x3038] attrs

-- * Initialization & Terminating
eglGetDefaultDisplay :: IO EGLDisplay
eglGetDefaultDisplay = EGLDisplay <$> c_eglGetDisplay nullPtr

eglGetDisplay :: EGLNativeDisplay a => a -> IO EGLDisplay
eglGetDisplay display = EGLDisplay <$> c_eglGetDisplay (getNativeDisplay display)

eglInitialize :: EGLDisplay -> EGL (Int, Int) -- ^ EGL (major, minor) version
eglInitialize display = alloca $ \major -> alloca $ \minor ->
  checkBool (c_eglInitialize (unD display) major minor) ((,) <$> peek major <*> peek minor)

eglTerminate :: EGLDisplay -> IO EGLError
eglTerminate display = toEglErr $ c_eglTerminate (unD display)

eglVendor display = c_eglQueryString (unD display) 0x3053 >>= peekCString
eglVersion display = c_eglQueryString (unD display) 0x3054 >>= peekCString
eglExtensions display = words <$> (c_eglQueryString (unD display) 0x3055 >>= peekCString)
eglClientAPIs display = words <$> (c_eglQueryString (unD display) 0x308D >>= peekCString)

eglQueryString :: EGLDisplay -> Int -> EGL String
eglQueryString display name =
  checkErr (c_eglQueryString (unD display) name) (/= nullPtr) (\str -> Right <$> peekCString str)

eglReleaseThread :: IO EGLError
eglReleaseThread = toEglErr c_eglReleaseThread

-- * Configuration Management
eglGetConfigs :: EGLDisplay -> EGL [EGLConfig]
eglGetConfigs display = alloca $ \num_config ->
  checkErr (c_eglGetConfigs (unD display) nullPtr 0 num_config) isTrue $ \_ -> do
    n <- peek num_config
    allocaArray n $ \configs ->
      checkBool (c_eglGetConfigs (unD display) configs n num_config) (map EGLConfig <$> peekArray n configs)

eglChooseConfig :: EGLDisplay -> [(EGLConfAttr, Int)] -> EGL [EGLConfig]
eglChooseConfig display attrs = withArray (const [0x3038] attrs) $ \attrib_list -> alloca $ \num_config ->
  checkErr (c_eglChooseConfig (unD display) attrib_list nullPtr 0 num_config) isTrue $ \_ -> do
    n <- peek num_config
    allocaArray n $ \configs ->
      checkBool (c_eglChooseConfig (unD display) attrib_list configs n num_config) (map EGLConfig <$> peekArray n configs)

eglGetConfigAttrib :: EGLDisplay -> EGLConfig -> EGLConfAttr -> EGL Int
eglGetConfigAttrib display config attribute = alloca $ \value ->
  checkBool (c_eglGetConfigAttrib (unD display) (unC config) (fromEnum attribute) value) (peek value)

-- * Rendering Surfaces
-- include EGL_RENDER_BUFFER,EGL_VG_COLORSPACE, and EGL_VG_ALPHA_FORMAT + ext
eglCreateWindowSurface :: EGLNativeWindow a => EGLDisplay -> EGLConfig -> a -> [(EGLSurfAttr, Int)] -> EGL EGLSurface
eglCreateWindowSurface display config win attrs =
  withAttrList attrs $ \attrib_list ->
    checkPtr (c_eglCreateWindowSurface (unD display) (unC config) (getNativeWindow win) attrib_list) EGLSurface

-- include EGL_WIDTH,EGL_HEIGHT,EGL_LARGEST_PBUFFER,EGL_TEXTURE_FORMAT,EGL_TEXTURE_TARGET,
-- EGL_MIPMAP_TEXTURE,EGL_VG_COLORSPACE, and EGL_VG_ALPHA_FORMAT + ext
eglCreatePbufferSurface :: EGLDisplay -> EGLConfig -> [(EGLSurfAttr, Int)] -> EGL EGLSurface
eglCreatePbufferSurface display config attrs =
  withAttrList attrs $ \attrib_list ->
    checkPtr (c_eglCreatePbufferSurface (unD display) (unC config) attrib_list) EGLSurface

-- include EGL_TEXTURE_FORMAT,EGL_TEXTURE_TARGET, and EGL_MIPMAP_TEXTURE
eglCreatePbufferFromClientBuffer :: EGLDisplay -> EGLenum -> EGLClientBuffer -> EGLConfig -> [(EGLSurfAttr, Int)] -> EGL EGLSurface
eglCreatePbufferFromClientBuffer display buftype buffer config attrs =
  withAttrList attrs $ \attrib_list ->
    checkPtr (c_eglCreatePbufferFromClientBuffer (unD display) buftype (unB buffer) (unC config) attrib_list) EGLSurface

eglDestroySurface :: EGLDisplay -> EGLSurface -> IO EGLError
eglDestroySurface display surface = toEglErr (c_eglDestroySurface (unD display) (unS surface))

-- include EGL_VG_COLORSPACE and EGL_VG_ALPHA_FORMAT + ext
eglCreatePixmapSurface :: EGLNativePixmap a => EGLDisplay -> EGLConfig -> a ->  [(EGLSurfAttr, Int)] -> EGL EGLSurface
eglCreatePixmapSurface display config pixmap attrs =
  withAttrList attrs $ \attrib_list ->
    checkPtr (c_eglCreatePixmapSurface (unD display) (unC config) (getNativePixmap pixmap) attrib_list) EGLSurface

eglSurfaceAttrib :: EGLDisplay -> EGLSurface -> EGLSurfAttr -> Int -> IO EGLError
eglSurfaceAttrib display surface attribute value =
  toEglErr (c_eglSurfaceAttrib (unD display) (unS surface) (fromEnum attribute) value)

eglQuerySurface :: EGLDisplay -> EGLSurface -> EGLSurfAttr -> EGL Int
eglQuerySurface display surface attribute = alloca $ \value ->
  checkBool (c_eglQuerySurface (unD display) (unS surface) (fromEnum attribute) value) (peek value)

-- * Rendering Contexts
-- 0x30A0 | 0x30A1 | 0x30A2 | 0x3038
data EGLBindAPIValue = EGLOpenGLESAPI | EGLOpenVGAPI | EGLOpenGLAPI | EGLAPINone
instance Enum EGLBindAPIValue where
  fromEnum EGLOpenGLESAPI = 0x30A0
  fromEnum EGLOpenVGAPI = 0x30A1
  fromEnum EGLOpenGLAPI = 0x30A2
  fromEnum EGLAPINone = 0x3038
  toEnum 0x30A0 = EGLOpenGLESAPI
  toEnum 0x30A1 = EGLOpenVGAPI
  toEnum 0x30A2 = EGLOpenGLAPI
  toEnum _ = EGLAPINone

eglBindAPI :: EGLBindAPIValue -> IO EGLError
eglBindAPI value = toEglErr (c_eglBindAPI (fromEnum value))

eglQueryAPI :: IO EGLBindAPIValue
eglQueryAPI = toEnum <$> c_eglQueryAPI

-- only EGL_CONTEXT_CLIENT_VERSION 0x3098 = 1 or 2
data EGLContextAttr = EGLContextClientVersion
instance Enum EGLContextAttr where
  fromEnum EGLContextClientVersion = 0x3098
eglCreateContext :: EGLDisplay -> EGLConfig -> [(EGLContextAttr, Int)] -> EGL EGLContext
eglCreateContext display config attrs =
  eglCreateContextWithShareContext display config (EGLContext nullPtr) attrs

eglCreateContextWithShareContext :: EGLDisplay -> EGLConfig -> EGLContext -> [(EGLContextAttr, Int)] -> EGL EGLContext
eglCreateContextWithShareContext display config shared_cxt attrs =
  withAttrList attrs $ \attrib_list ->
     checkPtr (c_eglCreateContext (unD display) (unC config) (unX shared_cxt) attrib_list) EGLContext

eglDestroyContext :: EGLDisplay -> EGLContext -> IO EGLError
eglDestroyContext display context =
  toEglErr(c_eglDestroyContext (unD display) (unX context))

eglMakeCurrent :: EGLDisplay -> EGLSurface -> EGLSurface -> EGLContext -> IO EGLError
eglMakeCurrent display draw read context =
  toEglErr (c_eglMakeCurrent (unD display) (unS draw) (unS read) (unX context))

eglReleaseCurrent :: EGLDisplay -> IO EGLError
eglReleaseCurrent display = toEglErr (c_eglMakeCurrent (unD display) nullPtr nullPtr nullPtr)

eglGetCurrentContext :: EGL EGLContext
eglGetCurrentContext = checkPtr c_eglGetCurrentContext EGLContext

data EGLReadDraw = EGLRead | EGLDraw deriving Eq
eglGetCurrentSurface :: EGLReadDraw -> EGL EGLSurface
eglGetCurrentSurface readdraw =
  checkPtr (c_eglGetCurrentSurface $
      if readdraw == EGLDraw then 0x3059 else 0x305A) EGLSurface

eglGetCurrentDisplay :: EGL EGLDisplay
eglGetCurrentDisplay = checkPtr c_eglGetCurrentDisplay EGLDisplay

-- EGL_CONFIG_ID, EGL_CONTEXT_CLIENT_TYPE, EGL_CONTEXT_CLIENT_VERSION, EGL_RENDER_BUFFER
eglConfigID d c = eglQueryContext d c 0x3028
eglContextClientType d c = eglQueryContext d c 0x3097
eglContextClientVersion d c = eglQueryContext d c 0x3098
eglRenderBuffer d c = eglQueryContext d c 0x3086

eglQueryContext :: EGLDisplay -> EGLContext -> Int -> EGL Int
eglQueryContext display context attribute = alloca $ \value ->
  checkBool (c_eglQueryContext (unD display) (unX context) attribute value) (peek value)

-- * Synchronization Primitives
eglWaitClient :: IO EGLError
eglWaitClient = toEglErr c_eglWaitClient
-- eglWaitGL is available for backwards compatibility
-- eglWaitGL :: IO EGLError
-- eglWaitGL = toEglErr c_eglWaitGL

data EGLWaitEngine = EGLCoreNativeEngine
eglWaitNative :: EGLWaitEngine -> IO EGLError
eglWaitNative engine = toEglErr (c_eglWaitNative 0x305B)

-- * Posting the Color Buffer
eglSwapBuffers :: EGLDisplay -> EGLSurface -> IO EGLError
eglSwapBuffers display surface =
  toEglErr (c_eglSwapBuffers (unD display) (unS surface))

eglCopyBuffers :: EGLNativePixmap a => EGLDisplay -> EGLSurface -> a -> IO EGLError
eglCopyBuffers display surface pixmap =
  toEglErr (c_eglCopyBuffers (unD display) (unS surface) (getNativePixmap pixmap))

eglSwapInterval :: EGLDisplay -> Int -> IO EGLError
eglSwapInterval display interval =
  toEglErr (c_eglSwapInterval (unD display) interval)

-- * Render to Textures
-- EGL_RENDER_BUFFER value 0x3084 | 0x3085
data EGLBuffer = EGLBackBuffer | EGLSingleBuffer deriving Eq
eglBindTexImage :: EGLDisplay -> EGLSurface -> EGLBuffer -> IO EGLError
eglBindTexImage display surface buffer =
  toEglErr (c_eglBindTexImage (unD display) (unS surface) (if buffer == EGLBackBuffer then 0x3084 else 0x3085))

eglReleaseTexImage :: EGLDisplay -> EGLSurface -> EGLBuffer -> IO EGLError
eglReleaseTexImage display surface buffer =
  toEglErr (c_eglReleaseTexImage (unD display) (unS surface) (if buffer == EGLBackBuffer then 0x3084 else 0x3085))

-- * Obtain Extension Function Pointers
eglGetProcAddress :: String -> FunPtr a
eglGetProcAddress procname =
  unsafePerformIO $ withCString procname c_eglGetProcAddress

#if defined(__GLASGOW_HASKELL__)
{-# INLINE [0] eglGetProcAddress #-}
{-# RULES
"EGL eglGetProcAddress/c_eglGetProcAddress" forall s .
   eglGetProcAddress (unpackCString# s) = inlinePerformIO (c_eglGetProcAddress (Ptr s))
 #-}

{-# INLINE inlinePerformIO #-}
-- | Just like unsafePerformIO, but we inline it. Big performance gains as
-- it exposes lots of things to further inlining. /Very unsafe/. In
-- particular, you should do no memory allocation inside an
-- 'inlinePerformIO' block.
--
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
#endif


-- * Extending EGL
{-
-- * Helper functions
-- | Validate the current internal state.
-- may return EGL_BAD_CONTEXT, EGL_BAD_SURFACE, EGL_BAD_NATIVE_WINDOW,
-- EGL_BAD_CURRENT_SURFACE, EGL_CONTEXT_LOST, EGL_BAD_DISPLAY,
-- EGL_NOT_INITIALIZED
testEGL :: IO EGLError
testEGL = do
  disp <- c_eglGetCurrentDisplay
  cont <- c_eglGetCurrentContext
  draw <- c_eglGetCurrentSurface EGLDraw
  toEglErr (c_eglMakeCurrent disp draw draw cont)
data EGLState a = EGLState (Maybe a) EGLConfig [(EGLSurfAttr, Int)]
eglState = unsafePerformIO $ newIORef (EGLState Nothing) (EGLConfig nullPtr) []

-- | Initialize an EGL Context for OpenGL ES 2.0 (eglGetDefaultDisplay - eglMakeCurrent) 
initEGL :: EGLNativeWindow a => a -> [(EGLConfAttr, Int)]
  -> [(EGLSurfAttr, Int)] -> IO EGLError
initEGL win confAttrs surfAttrs = do
  disp <- eglGetDefaultDisplay
  Right (major, minor) <- eglInitialize disp
  Right (config : _) <- eglChooseConfig disp
    ((EGLRenderableType, egl_opengl_es2_bit) : confAttrs)
  -- Request OpenGL ES 2.0+
  Right cont <- eglCreateContext disp config [(EGLContextClientVersion, 2)]
  Right surf <- eglCreateWindowSurface disp config win surfAttrs
  eglMakeCurrent disp surf surf cont
  writeIORef eglState $ EGLState win config surfAttrs

-- | Tear down the EGL context in use (eglMakeCurrent - eglTerminate)
termEGL :: IO EGLError
termEGL = do
  disp <- eglGetCurrentDisplay
  cont <- eglGetCurrentContext
  surf <- eglGetCurrentSurface EGLDraw
  either return (\disp -> do
    eglReleaseCurrent disp
    either return (eglDestroyContext disp) cont
    either return (eglDestroySurface disp) surf
    eglTerminate disp
    ) disp

-- | Short hand for eglSwapBuffers
swapCurrentBuffers :: IO EGLError
swapCurrentBuffers = do
  disp <- eglGetCurrentDisplay
  surf <- eglGetCurrentSurface EGLDraw
  err <- either return (\disp -> either return (\surf -> eglSwapBuffers disp surf) surf) disp
  case err of
    EGLSuccess -> return EGLSuccess
    -- still consider context is valid
    EGLBadSurface -> initEGLSurface >> return EGLSuccess where
      initEGLSurface = do
        EGLState win config surfAttrs <- readIORef eglState
        read <- eglGetCurrentSurface EGLRead
        Right surf <- eglCreateWindowSurface disp config win surfAttrs
        Right cont <- eglGetCurrentContext
        eglMakeCurrent disp surf (either (const surf) id read) cont
    -- EGLBadContext | EGLContextLost
    _ -> termEGL >> initEGL win >> return err

-- | Destroy current draw surface
suspendEGL :: IO EGLError
suspendEGL = do
  disp <- eglGetCurrentDisplay
  surf <- eglGetCurrentSurface EGLDraw
  either return (\disp -> either return (eglDestroySurface disp) surf) disp

resumeEGL :: EGLNativeWindow => a -> IO EGLError
resumeEGL win = do
  err <- eglGetCurrentContext
  err' <- case err of
    Left _ -> initEGL win
    Right _ -> initEGLSurface where
      initEGLSurface = do
        Right (config : _) <- eglChooseConfig disp [(EGLRenderableType, egl_opengl_es2_bit)]
        Right cont <- eglGetCurrentContext
        Right surf <- eglCreateWindowSurface disp config win []
        eglMakeCurrent disp surf surf cont 
  case err' of
    EGLSuccess -> return ()
    --EGLContextLost -> initEGLContext
    _ -> termEGL >> initEGL win
  return err'
-}
withCurrent :: (EGLError -> IO a) -- ^ Error handler
   -> (EGLDisplay -> EGLContext -> EGLSurface -> EGLSurface -> IO a)
   -- ^ Current Display in use -> Current Context -> Current Read Surface -> Current Draw Surface
   -> IO a
withCurrent f g = do
  disp <- eglGetCurrentDisplay
  cont <- eglGetCurrentContext
  read <- eglGetCurrentSurface EGLRead
  draw <- eglGetCurrentSurface EGLDraw
  either f (\d->
    either f (\c->
      either f (\r->
        either f (\s->
          g d c r s) draw) read) cont) disp

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
type EGLBoolean = Int -- actually, Word32
type EGLenum = Int -- actually, Word32
type EGLConfig_ = Ptr ()
type EGLContext_ = Ptr ()
type EGLDisplay_ = Ptr ()
type EGLSurface_ = Ptr ()
type EGLClientBuffer_ = Ptr ()
 
newtype EGLConfig = EGLConfig { unC :: EGLConfig_ }
newtype EGLContext = EGLContext { unX :: EGLContext_ }
newtype EGLDisplay = EGLDisplay { unD :: EGLDisplay_ }
newtype EGLSurface = EGLSurface { unS :: EGLSurface_ }
newtype EGLClientBuffer = EGLClientBuffer { unB :: EGLClientBuffer_ }

-- * EGL Functions
-- ccall unsafe: For more efficient code. These C calls do not call Haskell.
foreign import ccall unsafe "EGL/egl.h eglGetError" c_eglGetError :: IO EGLint

foreign import ccall unsafe "EGL/egl.h eglGetDisplay" c_eglGetDisplay :: EGLNativeDisplayType -> IO EGLDisplay_
foreign import ccall unsafe "EGL/egl.h eglInitialize" c_eglInitialize :: EGLDisplay_ -> Ptr EGLint -> Ptr EGLint -> IO EGLBoolean
foreign import ccall unsafe "EGL/egl.h eglTerminate" c_eglTerminate :: EGLDisplay_ -> IO EGLBoolean

foreign import ccall unsafe "EGL/egl.h eglQueryString" c_eglQueryString :: EGLDisplay_ -> EGLint -> IO CString

foreign import ccall unsafe "EGL/egl.h eglGetConfigs" c_eglGetConfigs :: EGLDisplay_ -> Ptr EGLConfig_ -> EGLint -> Ptr EGLint -> IO EGLBoolean
foreign import ccall unsafe "EGL/egl.h eglChooseConfig" c_eglChooseConfig :: EGLDisplay_ -> Ptr EGLint -> Ptr EGLConfig_ -> EGLint -> Ptr EGLint -> IO EGLBoolean
foreign import ccall unsafe "EGL/egl.h eglGetConfigAttrib" c_eglGetConfigAttrib :: EGLDisplay_ -> EGLConfig_ -> EGLint -> Ptr EGLint -> IO EGLBoolean

foreign import ccall unsafe "EGL/egl.h eglCreateWindowSurface" c_eglCreateWindowSurface :: EGLDisplay_ -> EGLConfig_ -> EGLNativeWindowType -> Ptr EGLint -> IO EGLSurface_
foreign import ccall unsafe "EGL/egl.h eglCreatePbufferSurface" c_eglCreatePbufferSurface :: EGLDisplay_ -> EGLConfig_ -> Ptr EGLint -> IO EGLSurface_
foreign import ccall unsafe "EGL/egl.h eglCreatePixmapSurface" c_eglCreatePixmapSurface :: EGLDisplay_ -> EGLConfig_ -> EGLNativePixmapType -> Ptr EGLint -> IO EGLSurface_
foreign import ccall unsafe "EGL/egl.h eglDestroySurface" c_eglDestroySurface :: EGLDisplay_ -> EGLSurface_ -> IO EGLBoolean
foreign import ccall unsafe "EGL/egl.h eglQuerySurface" c_eglQuerySurface :: EGLDisplay_ -> EGLSurface_ -> EGLint -> Ptr EGLint -> IO EGLBoolean

foreign import ccall unsafe "EGL/egl.h eglBindAPI" c_eglBindAPI :: EGLenum -> IO EGLBoolean
foreign import ccall unsafe "EGL/egl.h eglQueryAPI" c_eglQueryAPI :: IO EGLenum

foreign import ccall unsafe "EGL/egl.h eglWaitClient" c_eglWaitClient :: IO EGLBoolean

foreign import ccall unsafe "EGL/egl.h eglReleaseThread" c_eglReleaseThread :: IO EGLBoolean

foreign import ccall unsafe "EGL/egl.h eglCreatePbufferFromClientBuffer" c_eglCreatePbufferFromClientBuffer :: EGLDisplay_ -> EGLenum -> EGLClientBuffer_ -> EGLConfig_ -> Ptr EGLint -> IO EGLSurface_

foreign import ccall unsafe "EGL/egl.h eglSurfaceAttrib" c_eglSurfaceAttrib :: EGLDisplay_ -> EGLSurface_ -> EGLint -> EGLint -> IO EGLBoolean
foreign import ccall unsafe "EGL/egl.h eglBindTexImage" c_eglBindTexImage :: EGLDisplay_ -> EGLSurface_ -> EGLint -> IO EGLBoolean
foreign import ccall unsafe "EGL/egl.h eglReleaseTexImage" c_eglReleaseTexImage :: EGLDisplay_ -> EGLSurface_ -> EGLint -> IO EGLBoolean

foreign import ccall unsafe "EGL/egl.h eglSwapInterval" c_eglSwapInterval :: EGLDisplay_ -> EGLint -> IO EGLBoolean

foreign import ccall unsafe "EGL/egl.h eglCreateContext" c_eglCreateContext :: EGLDisplay_ -> EGLConfig_ -> EGLContext_ -> Ptr EGLint -> IO EGLContext_
foreign import ccall unsafe "EGL/egl.h eglDestroyContext" c_eglDestroyContext :: EGLDisplay_ -> EGLContext_ -> IO EGLBoolean
foreign import ccall unsafe "EGL/egl.h eglMakeCurrent" c_eglMakeCurrent :: EGLDisplay_ -> EGLSurface_ -> EGLSurface_ -> EGLContext_ -> IO EGLBoolean

foreign import ccall unsafe "EGL/egl.h eglGetCurrentContext" c_eglGetCurrentContext :: IO EGLContext_
foreign import ccall unsafe "EGL/egl.h eglGetCurrentSurface" c_eglGetCurrentSurface :: EGLint -> IO EGLSurface_
foreign import ccall unsafe "EGL/egl.h eglGetCurrentDisplay" c_eglGetCurrentDisplay :: IO EGLDisplay_
foreign import ccall unsafe "EGL/egl.h eglQueryContext" c_eglQueryContext :: EGLDisplay_ -> EGLContext_ -> EGLint -> Ptr EGLint -> IO EGLBoolean

--foreign import ccall unsafe "EGL/egl.h eglWaitGL" c_eglWaitGL :: IO EGLBoolean
foreign import ccall unsafe "EGL/egl.h eglWaitNative" c_eglWaitNative :: EGLint -> IO EGLBoolean
foreign import ccall unsafe "EGL/egl.h eglSwapBuffers" c_eglSwapBuffers :: EGLDisplay_ -> EGLSurface_ -> IO EGLBoolean
foreign import ccall unsafe "EGL/egl.h eglCopyBuffers" c_eglCopyBuffers :: EGLDisplay_ -> EGLSurface_ -> EGLNativePixmapType -> IO EGLBoolean

foreign import ccall unsafe "EGL/egl.h eglGetProcAddress" c_eglGetProcAddress :: CString -> IO (FunPtr a)
