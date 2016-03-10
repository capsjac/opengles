{-# LANGUAGE CPP #-}
-- | Low-level wrapper and some utility around EGL upto 1.5.
-- "Graphics.EGL" is preffered for normal use.
module Graphics.EGL.Base where

import Control.Monad (when)
import Data.IORef
import Foreign
import Foreign.C.String
import Graphics.OpenGLES.Base.Proc (glGetProcAddress)
import Graphics.OpenGLES.Internal (glLog)


-- * EGL Types

-- | 32-bit signed integer.
type EGLint = Int32

-- from EGL/egl.h
type EGLboolean = Word32
type EGLenum = Word32
type EGLConfig = Ptr ()
type EGLContext = Ptr ()
type EGLDisplay = Ptr ()
type EGLSurface = Ptr ()
type EGLClientBuffer = Ptr ()

-- from EGL/eglplatform.h
-- ** Window System Types

type EGLNativeWindow = Ptr ()
type EGLNativePixmap = Ptr ()
-- Symbian platform uses int instead of pointer
type EGLNativeDisplay = Ptr ()

-- EGL 1.5 Types
type EGLAttrib = Ptr EGLint
type EGLImage = Ptr ()
type EGLTime = Int64
type EGLSync = Ptr ()


-- * Bindings to EGL

#ifndef NOEGL

#define EGL14(_name, _type) \
foreign import ccall unsafe "EGL/egl.h" _name :: _type; \

#define EGL15(_name, _vendor, _type) \
foreign import ccall unsafe "dynamic" unwrap_/**/_name :: FunPtr (_type) -> _type; \
_name :: _type; \
_name = unwrap_/**/_name (glGetProcAddress "_name/**/_vendor"); \

#define EGLEXT(_name, _type) EGL15(_name/**/_vendor, _type)

#ifdef STATIC_EGL15
#define EGL15(_name, _vendor, _type) EGL14(_name, _type)
#endif

#else

#define EGL14(_name, _type) \
_name :: _type; \
_name = error "EGL is unsupported on this platform"; \

#define EGL15(_a, _b, _c) EGL14(_a, _c)
#define EGLEXT EGL14

#endif


-- ** EGL 1.4

EGL14(eglGetError, IO EGLint)
EGL14(eglGetDisplay, EGLNativeDisplay -> IO EGLDisplay)
EGL14(eglInitialize, EGLDisplay -> EGLAttrib -> EGLAttrib -> IO EGLboolean)
EGL14(eglTerminate, EGLDisplay -> IO EGLboolean)

EGL14(eglQueryString, EGLDisplay -> EGLint -> IO CString)

EGL14(eglGetConfigs, EGLDisplay -> Ptr EGLConfig -> EGLint -> EGLAttrib -> IO EGLboolean)
EGL14(eglChooseConfig, EGLDisplay -> EGLAttrib -> Ptr EGLConfig -> EGLint -> EGLAttrib -> IO EGLboolean)
EGL14(eglGetConfigAttrib, EGLDisplay -> EGLConfig -> EGLint -> EGLAttrib -> IO EGLboolean)

EGL14(eglCreateWindowSurface, EGLDisplay -> EGLConfig -> EGLNativeWindow -> EGLAttrib -> IO EGLSurface)
EGL14(eglCreatePbufferSurface, EGLDisplay -> EGLConfig -> EGLAttrib -> IO EGLSurface)
EGL14(eglCreatePixmapSurface, EGLDisplay -> EGLConfig -> EGLNativePixmap -> EGLAttrib -> IO EGLSurface)
EGL14(eglDestroySurface, EGLDisplay -> EGLSurface -> IO EGLboolean)
EGL14(eglQuerySurface, EGLDisplay -> EGLSurface -> EGLint -> EGLAttrib -> IO EGLboolean)

EGL14(eglBindAPI, EGLenum -> IO EGLboolean)
EGL14(eglQueryAPI, IO EGLenum)

EGL14(eglWaitClient, IO EGLboolean)

EGL14(eglReleaseThread, IO EGLboolean)

EGL14(eglCreatePbufferFromClientBuffer, EGLDisplay -> EGLenum -> EGLClientBuffer -> EGLConfig -> EGLAttrib -> IO EGLSurface)

EGL14(eglSurfaceAttrib, EGLDisplay -> EGLSurface -> EGLint -> EGLint -> IO EGLboolean)
EGL14(eglBindTexImage, EGLDisplay -> EGLSurface -> EGLint -> IO EGLboolean)
EGL14(eglReleaseTexImage, EGLDisplay -> EGLSurface -> EGLint -> IO EGLboolean)

EGL14(eglSwapInterval, EGLDisplay -> EGLint -> IO EGLboolean)

EGL14(eglCreateContext, EGLDisplay -> EGLConfig -> EGLContext -> EGLAttrib -> IO EGLContext)
EGL14(eglDestroyContext, EGLDisplay -> EGLContext -> IO EGLboolean)
EGL14(eglMakeCurrent, EGLDisplay -> EGLSurface -> EGLSurface -> EGLContext -> IO EGLboolean)

EGL14(eglGetCurrentContext, IO EGLContext)
EGL14(eglGetCurrentSurface, EGLint -> IO EGLSurface)
EGL14(eglGetCurrentDisplay, IO EGLDisplay)
EGL14(eglQueryContext, EGLDisplay -> EGLContext -> EGLint -> EGLAttrib -> IO EGLboolean)

--EGL14(eglWaitGL, IO EGLboolean)
EGL14(eglWaitNative, EGLint -> IO EGLboolean)
EGL14(eglSwapBuffers, EGLDisplay -> EGLSurface -> IO EGLboolean)
EGL14(eglCopyBuffers, EGLDisplay -> EGLSurface -> EGLNativePixmap -> IO EGLboolean)

--EGL14(eglGetProcAddress, CString -> IO (FunPtr a))

-- ** EGL 1.5
-- EGL_KHR_gl_colorspace
-- #define EGL_GL_COLORSPACE                 0x309D -- eglCreateWindowSurface, eglCreatePbufferSurface and eglCreatePixmapSurface attrib
-- #define EGL_GL_COLORSPACE_SRGB            0x3089 -- value
-- #define EGL_GL_COLORSPACE_LINEAR          0x308A -- value

-- EGL_KHR_gl_image family (use with eglCreateImageKHR)
-- - EGL_KHR_gl_texture_2D_image
#define EGL_GL_TEXTURE_2D                 0x30B1
#define EGL_GL_TEXTURE_LEVEL              0x30BC -- attrib
-- - EGL_KHR_gl_texture_cubemap_image
#define EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_X 0x30B3
#define EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_X 0x30B4
#define EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Y 0x30B5
#define EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Y 0x30B6
#define EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Z 0x30B7
#define EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Z 0x30B8
-- - EGL_KHR_gl_texture_3D_image
#define EGL_GL_TEXTURE_3D                 0x30B2
#define EGL_GL_TEXTURE_ZOFFSET            0x30BD -- attrib
-- - EGL_KHR_gl_renderbuffer_image
#define EGL_GL_RENDERBUFFER               0x30B9

-- EGL_KHR_create_context slightly modified in 1.5
-- #define EGL_CONTEXT_MAJOR_VERSION         0x3098
-- #define EGL_CONTEXT_MINOR_VERSION         0x30FB
-- removed in 1.5: EGL_CONTEXT_FLAGS_KHR 0x30FC and related bits
-- #define EGL_CONTEXT_OPENGL_PROFILE_MASK   0x30FD
-- #define EGL_CONTEXT_OPENGL_RESET_NOTIFICATION_STRATEGY 0x31BD
-- #define EGL_NO_RESET_NOTIFICATION         0x31BE
-- #define EGL_LOSE_CONTEXT_ON_RESET         0x31BF
-- #define EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT 0x00000001
-- #define EGL_CONTEXT_OPENGL_COMPATIBILITY_PROFILE_BIT 0x00000002
-- #define EGL_CONTEXT_OPENGL_DEBUG          0x31B0 or 1
-- #define EGL_CONTEXT_OPENGL_FORWARD_COMPATIBLE 0x31B1 or 2
-- #define EGL_CONTEXT_OPENGL_ROBUST_ACCESS  0x31B2 or 4
-- #define EGL_OPENGL_ES3_BIT                0x00000040

-- EGL_KHR_fence_sync (extends EGL_KHR_reusable_sync)
#define EGL_SYNC_PRIOR_COMMANDS_COMPLETE_KHR	0x30F0
#define EGL_SYNC_CONDITION_KHR			0x30F8
#define EGL_SYNC_FENCE_KHR			0x30F9

-- EGL_KHR_reusable_sync (exclude eglSignalSyncKHR)
#define EGL_SYNC_STATUS_KHR			0x30F1
#define EGL_SIGNALED_KHR			0x30F2
#define EGL_UNSIGNALED_KHR			0x30F3
#define EGL_TIMEOUT_EXPIRED_KHR			0x30F5
#define EGL_CONDITION_SATISFIED_KHR		0x30F6
#define EGL_SYNC_TYPE_KHR			0x30F7
-- not in 1.5 #define EGL_SYNC_REUSABLE_KHR			0x30FA
#define EGL_SYNC_FLUSH_COMMANDS_BIT_KHR		0x0001	/* eglClientWaitSyncKHR <flags> bitfield */
#define EGL_FOREVER_KHR				0xFFFFFFFFFFFFFFFFull
#define EGL_NO_SYNC_KHR				((EGLSyncKHR)0)
EGL15(eglCreateSync,KHR, EGLDisplay -> EGLenum -> EGLAttrib -> IO EGLSync)
EGL15(eglDestroytSync,KHR, EGLDisplay -> EGLSync -> IO EGLboolean)
EGL15(eglGetSyncAttrib,KHR, EGLDisplay -> EGLSync -> EGLint -> EGLAttrib -> IO EGLboolean)
EGL15(eglClientWaitSync,KHR, EGLDisplay -> EGLSync -> EGLint -> EGLTime -> IO EGLint)

-- EGL_KHR_wait_sync
EGL15(eglWaitSync,KHR, EGLDisplay -> EGLSync -> EGLint -> IO EGLboolean)

-- EGL_KHR_image_base (extends EGL_KHR_image_pixmap)
#define EGL_IMAGE_PRESERVED               0x30D2
-- EGL_KHR_image_pixmap (extends EGL_KHR_image)
-- EGL_KHR_image
-- not in 1.5 #define EGL_NATIVE_PIXMAP_KHR			0x30B0	/* eglCreateImageKHR target */
#define EGL_NO_IMAGE_KHR			((EGLImageKHR)0)
EGL15(eglCreateImage,KHR, EGLDisplay -> EGLContext -> EGLenum -> EGLClientBuffer -> EGLAttrib -> IO EGLImage)
EGL15(eglDestroytImage,KHR, EGLDisplay -> EGLImage -> IO EGLboolean)

-- EGL_EXT_platform_base (optionally EGL_EXT_client_extensions)
EGL15(eglGetPlatformDisplay,EXT, EGLenum -> EGLNativeDisplay -> EGLAttrib -> IO EGLDisplay)
EGL15(eglCreatePlatformWindowSurface,EXT, EGLDisplay -> EGLConfig -> EGLNativeWindow -> EGLAttrib -> IO EGLSurface)
EGL15(eglCreatePlatformPixmapSurface,EXT, EGLDisplay -> EGLConfig -> EGLNativePixmap -> EGLAttrib -> IO EGLSurface)


-- ** Extensions

-- EGL_EXT_platform_device (extends EGL_EXT_platform_base)
#define EGL_PLATFORM_DEVICE_EXT           0x313F
-- EGL_EXT_platform_wayland (extends EGL_EXT_platform_base)
#define EGL_PLATFORM_WAYLAND_EXT          0x31D8
-- EGL_EXT_platform_x11 (extends EGL_EXT_platform_base)
#define EGL_PLATFORM_X11_EXT              0x31D5 -- eglGetPlatformDisplayEXT platform
#define EGL_PLATFORM_X11_SCREEN_EXT       0x31D6 -- eglGetPlatformDisplayEXT attrib


-- * Misc

newtype EGLConfAttr = EGLConfAttr EGLint
newtype EGLSurfAttr = EGLSurfAttr EGLint
newtype EGLContextAttr = EGLContextAttr EGLint

{-# NOINLINE queryString #-}
queryString :: EGLint -> Egl -> IO String
queryString name egl = do
	display <- fmap disp $ readIORef egl
	eglQueryString display name >>= peekCString

{-# NOINLINE queryContext #-}
queryContext :: EGLint -> Egl -> IO EGLint
queryContext attr egl = do
	EglCurrent{disp=disp, context=context} <- readIORef egl
	alloca $ \value ->
		eglQueryContext disp context attr value >> peek value

-- | EGL state holder per context.
type Egl = IORef EglCurrent

data EglCurrent = EglCurrent
	{ disp :: EGLDisplay
	, chosen :: EGLConfig
	, context :: EGLContext
	, dsurf :: EGLSurface
	, rsurf :: EGLSurface
	, nwin :: EGLNativeWindow
	, screenDims :: (Int32, Int32)
	, ndisp :: Maybe EGLNativeDisplay
	, confcand :: [[(EGLConfAttr, Int32)]]
	--, surfconf :: [[(EGLSurfAttr, Int32)]]
	, cxtconf :: [(EGLContextAttr, Int32)]
	}

initial = EglCurrent nullPtr nullPtr nullPtr nullPtr nullPtr nullPtr (0,0)

{-# NOINLINE showEglError #-}
-- | Trun errno into String.
showEglError :: EGLint -> String
showEglError x = case x of
	0x3000 -> "EGLSuccess: Function succeeded."
	0x3001 -> "EGLNotInitialized: EGL is not or could not be initialized, for the specified display."
	0x3002 -> "EGLBadAccess: EGL cannot access a requested resource (for example, a context is bound in another thread)."
	0x3003 -> "EGLBadAlloc: EGL failed to allocate resources for the requested operation."
	0x3004 -> "EGLBadAttribute: An unrecognized attribute or attribute value was passed in an attribute list."
	0x3005 -> "EGLBadConfig: An EGLConfig argument does not name a valid EGLConfig."
	0x3006 -> "EGLBadContext: An EGLContext argument does not name a valid EGLContext."
	0x3007 -> "EGLBadCurrentSurface: The current surface of the calling thread is a window, pbuffer, or pixmap that is no longer valid."
	0x3008 -> "EGLBadDisplay: An EGLDisplay argument does not name a valid EGLDisplay."
	0x3009 -> "EGLBadMatch: Arguments are inconsistent; for example, an otherwise valid context requires buffers (e.g. depth or stencil) not allocated by an otherwise valid surface."
	0x300A -> "EGLBadNativePixmap: An EGLNativePixmapType argument does not refer to a valid native pixmap."
	0x300B -> "EGLBadNativeWindow: An EGLNativeWindowType argument does not refer to a valid native window."
	0x300C -> "EGLBadParameter: One or more argument values are invalid."
	0x300D -> "EGLBadSurface: An EGLSurface argument does not name a valid surface (window, pbuffer, or pixmap) configured for rendering."
	0x300E -> "EGLContextLost: A power management event has occurred. The application must destroy all contexts and reinitialise client API state and objects to continue rendering."
	x | 0x300E < x && x < 0x3020 ->
		"EGLUnknownError: Error " ++ show x ++ " is not defined in EGL 1.4 spec."
	x -> "showEglError: Value out of range: " ++ show x

logError :: String -> IO ()
logError location = do
	err <- eglGetError
	glLog (location ++ ": " ++ showEglError err)
{-# NOINLINE logError #-}

withErrorCheck :: String -> IO EGLboolean -> IO Bool
withErrorCheck loc io = do
	p <- io
	when (p == 0) $ logError loc
	return (p /= 0)

-- | Encode attribute list from [(name, value)] pairs.
withAttrList :: [(EGLint, Int32)] -> (Ptr EGLint -> IO b) -> IO b
withAttrList attrs =
	withArray $ foldr (\(k, v) l -> k : v : l) [0x3038] attrs
{-# NOINLINE withAttrList #-}

-- | Get an EGLDisplay and choose a suitable config.
setupEgl :: Maybe EGLNativeDisplay -> [[(EGLConfAttr, Int32)]] -> IO (EGLDisplay, EGLConfig)
setupEgl nd attribsList = do
	--display <- withAttrList [(0x3202, 0x3206){-, (0x3208, 1)-}] $
	--	eglGetPlatformDisplayEXT 0x3201 (maybe nullPtr id nd)
	display <- eglGetDisplay $ maybe nullPtr id nd
	logError "eglGetDisplay"
	eglInitialize display nullPtr nullPtr
	logError "eglInitialize"
	-- Here we specify the attributes of the desired configuration.
	-- Below, we select an EGLConfig with at least 8 bits per color
	-- component compatible with on-screen windows
	let go _ _ [] = return nullPtr -- suitable config not found!
	    go cfg numConfigs (x:xs) =
	    	withAttrList (map (\(EGLConfAttr a,b)->(a,b)) x) $ \attribs -> do
			eglChooseConfig display attribs cfg 1 numConfigs
			logError "eglChooseConfig"
			n <- peek numConfigs
			if n /= 0 then peek cfg else go cfg numConfigs xs
	config <- alloca $ \cfg ->
		alloca $ \numConfigs ->
			go cfg numConfigs attribsList

	-- EGL_NATIVE_VISUAL_ID is an attribute of the EGLConfig that is
	-- guaranteed to be accepted by ANativeWindow_setBuffersGeometry.
	-- As soon as we picked a EGLConfig, we can safely reconfigure the
	-- ANativeWindow buffers to match, using EGL_NATIVE_VISUAL_ID.
	--alloca $ \format->
	--	eglGetConfigAttrib display config EGL_NATIVE_VISUAL_ID format
	--	ANativeWindow_setBuffersGeometry window 0 0 format
	return (display, config)
{-# NOINLINE setupEgl #-}

-- | Create a surface and return its pointer, screen_width and screen_height.
setSurface :: EGLDisplay -> EGLConfig -> EGLNativeWindow -> IO (EGLSurface, EGLint, EGLint)
setSurface disp config window = do
	surf <- eglCreateWindowSurface disp config window nullPtr
	logError "eglCreateWindowSurface"
	alloca $ \ptr -> do
		eglQuerySurface disp surf 0x3057 ptr
		screen_width <- peek ptr
		eglQuerySurface disp surf 0x3056 ptr
		screen_height <- peek ptr
		return (surf, screen_width, screen_height)
{-# NOINLINE setSurface #-}

