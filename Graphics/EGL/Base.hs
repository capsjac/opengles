{-# LANGUAGE CPP #-}
{-# LANGUAGE Unsafe, UnliftedFFITypes, MagicHash, UnboxedTuples #-}
module Graphics.EGL.Base where

import Data.IORef
import Foreign
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)
import GHC.Base (realWorld#)
import GHC.CString (unpackCString#)
import GHC.IO (IO (IO))
import GHC.Ptr (Ptr(..))


-- * EGL Types

-- | 32-bit signed integer
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


-- * Bindings to EGL

#define EGL14(_name, _type) \
foreign import ccall unsafe "EGL/egl.h" _name :: _type

EGL14(eglGetError, IO EGLint)
EGL14(eglGetDisplay, EGLNativeDisplay -> IO EGLDisplay)
EGL14(eglInitialize, EGLDisplay -> Ptr EGLint -> Ptr EGLint -> IO EGLboolean)
EGL14(eglTerminate, EGLDisplay -> IO EGLboolean)

EGL14(eglQueryString, EGLDisplay -> EGLint -> IO CString)

EGL14(eglGetConfigs, EGLDisplay -> Ptr EGLConfig -> EGLint -> Ptr EGLint -> IO EGLboolean)
EGL14(eglChooseConfig, EGLDisplay -> Ptr EGLint -> Ptr EGLConfig -> EGLint -> Ptr EGLint -> IO EGLboolean)
EGL14(eglGetConfigAttrib, EGLDisplay -> EGLConfig -> EGLint -> Ptr EGLint -> IO EGLboolean)

EGL14(eglCreateWindowSurface, EGLDisplay -> EGLConfig -> EGLNativeWindow -> Ptr EGLint -> IO EGLSurface)
EGL14(eglCreatePbufferSurface, EGLDisplay -> EGLConfig -> Ptr EGLint -> IO EGLSurface)
EGL14(eglCreatePixmapSurface, EGLDisplay -> EGLConfig -> EGLNativePixmap -> Ptr EGLint -> IO EGLSurface)
EGL14(eglDestroySurface, EGLDisplay -> EGLSurface -> IO EGLboolean)
EGL14(eglQuerySurface, EGLDisplay -> EGLSurface -> EGLint -> Ptr EGLint -> IO EGLboolean)

EGL14(eglBindAPI, EGLenum -> IO EGLboolean)
EGL14(eglQueryAPI, IO EGLenum)

EGL14(eglWaitClient, IO EGLboolean)

EGL14(eglReleaseThread, IO EGLboolean)

EGL14(eglCreatePbufferFromClientBuffer, EGLDisplay -> EGLenum -> EGLClientBuffer -> EGLConfig -> Ptr EGLint -> IO EGLSurface)

EGL14(eglSurfaceAttrib, EGLDisplay -> EGLSurface -> EGLint -> EGLint -> IO EGLboolean)
EGL14(eglBindTexImage, EGLDisplay -> EGLSurface -> EGLint -> IO EGLboolean)
EGL14(eglReleaseTexImage, EGLDisplay -> EGLSurface -> EGLint -> IO EGLboolean)

EGL14(eglSwapInterval, EGLDisplay -> EGLint -> IO EGLboolean)

EGL14(eglCreateContext, EGLDisplay -> EGLConfig -> EGLContext -> Ptr EGLint -> IO EGLContext)
EGL14(eglDestroyContext, EGLDisplay -> EGLContext -> IO EGLboolean)
EGL14(eglMakeCurrent, EGLDisplay -> EGLSurface -> EGLSurface -> EGLContext -> IO EGLboolean)

EGL14(eglGetCurrentContext, IO EGLContext)
EGL14(eglGetCurrentSurface, EGLint -> IO EGLSurface)
EGL14(eglGetCurrentDisplay, IO EGLDisplay)
EGL14(eglQueryContext, EGLDisplay -> EGLContext -> EGLint -> Ptr EGLint -> IO EGLboolean)

--EGL14(eglWaitGL, IO EGLboolean)
EGL14(eglWaitNative, EGLint -> IO EGLboolean)
EGL14(eglSwapBuffers, EGLDisplay -> EGLSurface -> IO EGLboolean)
EGL14(eglCopyBuffers, EGLDisplay -> EGLSurface -> EGLNativePixmap -> IO EGLboolean)


-- * Obtain Extension Function Pointer

-- | Just like unsafePerformIO, but we inline it. Big performance gains as
-- it exposes lots of things to further inlining. /Very unsafe/. In
-- particular, you should do no memory allocation inside an
-- 'inlinePerformIO' block.
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
{-# INLINE inlinePerformIO #-}

foreign import ccall unsafe "EGL/egl.h eglGetProcAddress"
	c_eglGetProcAddress :: CString -> IO (FunPtr a)

eglGetProcAddress :: String -> FunPtr a
eglGetProcAddress procname = unsafePerformIO $
	withCString procname c_eglGetProcAddress
{-# INLINE [0] eglGetProcAddress #-}
{-# RULES
"EGL eglGetProcAddress/c_eglGetProcAddress" forall s .
   eglGetProcAddress (unpackCString# s) = inlinePerformIO (c_eglGetProcAddress (Ptr s))
 #-}


-- * Misc

newtype EGLConfAttr = EGLConfAttr EGLint
newtype EGLSurfAttr = EGLSurfAttr EGLint
newtype EGLContextAttr = EGLContextAttr EGLint

queryString :: EGLint -> Egl -> IO String
queryString name egl = do
	display <- fmap disp $ readIORef egl
	eglQueryString display name >>= peekCString

queryContext :: EGLint -> Egl -> IO EGLint
queryContext attr egl = do
	EglCurrent{disp=disp, context=context} <- readIORef egl
	alloca $ \value ->
		eglQueryContext disp context attr value >> peek value

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

withAttrList :: [(EGLint, Int32)] -> (Ptr EGLint -> IO b) -> IO b
withAttrList attrs =
	withArray $ foldr (\(k, v) l -> k : v : l) [0x3038] attrs

-- | Get an EGLDisplay and choose suitable configs
setupEgl :: Maybe EGLNativeDisplay -> [[(EGLConfAttr, Int32)]] -> IO (EGLDisplay, EGLConfig)
setupEgl nd attribsList = do
	display <- eglGetDisplay $ maybe nullPtr id nd
	eglInitialize display nullPtr nullPtr

	-- Here specify the attributes of the desired configuration.
	-- Below, we select an EGLConfig with at least 8 bits per color
	-- component compatible with on-screen windows
	let go _ _ [] = return nullPtr -- suitable config not found!
	    go cfg numConfigs (x:xs) = withAttrList (map (\(EGLConfAttr a,b)->(a,b))x) $ \attribs -> do
		eglChooseConfig display attribs cfg 1 numConfigs
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

setSurface :: EGLDisplay -> EGLConfig -> EGLNativeWindow -> IO (EGLSurface, EGLint, EGLint)
setSurface disp config window = do
	surf <- eglCreateWindowSurface disp config window nullPtr
	screen_width <- alloca $ \ptr ->
		eglQuerySurface disp surf 0x3057 ptr >> peek ptr
	screen_height <- alloca $ \ptr ->	
		eglQuerySurface disp surf 0x3056 ptr >> peek ptr
	return (surf, screen_width, screen_height)

