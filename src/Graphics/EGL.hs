{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
-- | EGL 1.5 carefully reconstructed.
-- EGL is an interface works between native window systems and rendering APIs
-- such as OpenGL, OpenGL ES, OpenVG.
-- This high-level APIs are specialized for OpenGL ES, so that initialization
-- could be simply done in several lines.
-- You can still access the rest of C APIs via "Graphics.EGL.Base".
-- 
-- - <http://www.khronos.org/registry/egl/ Khronos EGL Registry>
-- - <http://www.khronos.org/files/egl-1-4-quick-reference-card.pdf EGL 1.4 Quick Reference [PDF]>
module Graphics.EGL where

import Control.Applicative
import Control.Monad
import Data.IORef
import Foreign hiding (void)
import Graphics.EGL.Base
import Graphics.OpenGLES.Internal (glLog)

--init
--	setupEgl
--	setSurface
--	eglCreateContext
--	eglMakeCurrent
--eglswap
--	trySwap
--	BADSUR -> setupEgl, setSurface
--	CLOST, BADCXT -> reset
--resume
--	setSurface
--	eglMakeCurrent
--	CLOST -> eglCreateContext
--	OTHER -> reset


-- * EGL Lifecycle

-- | Initialize EGL with default display.
eglInit :: IO Egl
eglInit = eglInitializeOn Nothing
	[[(egl_RenderableType, egl_OpenGLES2)]]
	[(egl_ClientVersion, 2)]

-- | Initialize EGL with specified display and configurations.
eglInitializeOn
	:: Maybe EGLNativeDisplay -- ^ If any, specify native display pointer.
	-> [[(EGLConfAttr, Int32)]] -- ^ EGL configurations with many fallbacks.
	-> [(EGLContextAttr, Int32)] -- ^ Choose GL version here.
	-> IO Egl
eglInitializeOn nd cfgs = newIORef . initial nd cfgs

-- | Start using graphics APIs on this thread. initialization is performed here
-- whenever necessary. EGL context would be re-created if the last context was
-- lost due to power management events such as sleep.
eglResume
	:: Egl
	-> EGLNativeWindow -- ^ Pointer to the native window handler.
	-> IO ()
eglResume egl window = do
	glLog "eglResume..."
	cur@EglCurrent{..} <- readIORef egl
	let share = nullPtr
	if disp == nullPtr then do -- context not initialized
		glLog "egl.disp==NULL"
		(display, config) <- setupEgl ndisp confcand
		(surf, w, h) <- setSurface display config window
		cxt <- withAttrList (map (\(EGLContextAttr a,b)->(a,b))cxtconf) $
			eglCreateContext display config share
		logError "eglCreateContext(Init)"
		withErrorCheck "eglMakeCurrent(Init)" $
			eglMakeCurrent display surf surf cxt
		writeIORef egl cur
			{ disp = display
			, chosen = config
			, context = cxt
			, dsurf = surf
			, rsurf = surf
			, nwin = window
			, screenDims = (w, h) }
	else do -- resume
		glLog "egl.disp/=NULL"
		(surf, w, h) <- setSurface disp chosen window
		when (screenDims /= (w, h)) $
			glLog "screen resized"
		res <- eglMakeCurrent disp surf surf context
		when (res == 0) $ do -- EGL_FALSE
			err <- eglGetError
			glLog $ "eglMakeCurrent(Resume): " ++ showEglError err
			if err == 0x300E then do -- EGL_CONTEXT_LOST
				glLog "Re-creating EGL context..."
				cxt <- withAttrList (map (\(EGLContextAttr a,b)->(a,b))cxtconf) $
					eglCreateContext disp chosen share
				logError "eglCreateContext(Resume)"
				writeIORef egl cur
					{ context = cxt
					, dsurf = surf
					, rsurf = surf
					, nwin = window
					, screenDims = (w, h) }
			else do
				eglInvalidate egl
				-- context reset
				eglResume egl window
{-# NOINLINE eglResume #-}

-- | Stop using graphics APIs on this thread. Should be called before a sleep.
eglSuspend :: Egl -> IO ()
eglSuspend egl = do
	glLog "eglSuspend..."
	c@EglCurrent{..} <- readIORef egl
	when (dsurf /= nullPtr) $ do
		eglDestroySurface disp dsurf
		writeIORef egl c { dsurf = nullPtr }
{-# NOINLINE eglSuspend #-}

-- | Invalidate EGL objects and terminates the display on this thread.
-- You can still reuse 'Egl' because 'Egl' is state manager.
eglInvalidate :: Egl -> IO ()
eglInvalidate egl = do
	glLog "eglInvalidate..."
	EglCurrent{..} <- readIORef egl
	when (disp /= nullPtr) $ do
		eglMakeCurrent disp nullPtr nullPtr nullPtr
		when (context /= nullPtr) $
			void (eglDestroyContext disp context)
		when (dsurf /= nullPtr) $
			void (eglDestroySurface disp dsurf)
		when (rsurf /= nullPtr && rsurf /= dsurf) $
			void (eglDestroySurface disp rsurf)
		void $ eglTerminate disp
	writeIORef egl $ initial ndisp confcand cxtconf
{-# NOINLINE eglInvalidate #-}

--  | Create a shared context from given context.
--eglShareContext :: Egl -> IO Egl
--eglShareContext egl = do


-- * Posting the Color Buffer

-- | Post EGL surface color buffer to the native window.
-- eglPostFrame calls eglSwapBuffers inside.
eglPostFrame :: Egl -> IO ()
eglPostFrame egl = do
	cur@EglCurrent{..} <- readIORef egl
	b <- eglSwapBuffers disp dsurf
	when (b == 0) $ do
		err <- eglGetError
		glLog $ "eglPostBuffer: " ++ showEglError err
		if err == 0x300D then do -- EGL_BAD_SURFACE
			(display, config) <- setupEgl ndisp confcand
			-- recreate surface
			(surf, w, h) <- setSurface display config nwin
			-- still consider EGL context is valid
			writeIORef egl cur
				{ disp = display
				, dsurf = surf
				, screenDims = (w, h) }
		else do --if err == EGL_CONTEXT_LOST || err == EGL_BAD_CONTEXT then
			-- context has been lost!!
			eglInvalidate egl
			-- reset context
			eglResume egl nwin

--eglCopyBuffers :: EGLNativePixmap a => EGLDisplay -> EGLSurface -> a -> IO EGLError
--eglCopyBuffers display surface pixmap =
--  toEglErr (eglCopyBuffers (unD display) (unS surface) (getNativePixmap pixmap))

-- | Specifies the minimum number of video frame periods per buffer swap
-- for the window associated with the current context.
-- Returns False on failure. The default swap interval is 1.
eglPostInterval :: Egl -> Int32 -> IO Bool
eglPostInterval egl interval = do
	display <- disp <$> readIORef egl
	(/= 0) <$> eglSwapInterval display interval


-- * Query Context

eglVendor, eglVersion :: Egl -> IO String
eglVendor = queryString 0x3053
eglVersion = queryString 0x3054

eglExtensions, eglClientAPIs :: Egl -> IO [String]
eglExtensions egl = words <$> queryString 0x3055 egl
eglClientAPIs egl = words <$> queryString 0x308D egl

eglScreenDims :: Num a => Egl -> IO (a, a)
eglScreenDims egl = do
	(w, h) <- screenDims <$> readIORef egl
	return (fromIntegral w, fromIntegral h)


-- * Attributes

egl_True, egl_False, egl_None, egl_DontCare :: EGLint
egl_True = 1
egl_False = 0
egl_None = 0x3038
egl_DontCare = -1


-- ** Config Attributes

-- 0x3020-0x3042 (Reserved 0x3041-0x304F for additional config attributes)
egl_BufferSize, egl_AlphaSize, egl_BlueSize, egl_GreenSize, egl_RedSize,
    egl_DepthSize, egl_StencilSize, egl_ConfigCaveat, egl_ConfigID, egl_Level,
    egl_MaxPbufferHeight, egl_MaxPbufferPixels, egl_MaxPbufferWidth,
    egl_NativeRenderable, egl_NativeVisualID, egl_NativeVisualType, egl_Samples,
    egl_SampleBuffers, egl_SurfaceType, egl_TransparentType,
    egl_TransparentBlueValue, egl_TransparentGreenValue,
    egl_TransparentRedValue, egl_BindToTextureRGB, egl_BindToTextureRGBA,
    egl_MinSwapInterval, egl_MaxSwapInterval, egl_LuminanceSize,
    egl_AlphaMaskSize, egl_ColorBufferType, egl_RenderableType,
    egl_MatchNativePixmap, egl_Conformant
    :: EGLConfAttr
egl_BufferSize = EGLConfAttr 0x3020
egl_AlphaSize = EGLConfAttr 0x3021
egl_BlueSize = EGLConfAttr 0x3022
egl_GreenSize = EGLConfAttr 0x3023
egl_RedSize = EGLConfAttr 0x3024
egl_DepthSize = EGLConfAttr 0x3025
egl_StencilSize = EGLConfAttr 0x3026
-- | One of 'egl_None', 'egl_SlowConfig', 'egl_NonConformantConfig'.
egl_ConfigCaveat = EGLConfAttr 0x3027
egl_ConfigID = EGLConfAttr 0x3028
egl_Level = EGLConfAttr 0x3029
egl_MaxPbufferHeight = EGLConfAttr 0x302A
egl_MaxPbufferPixels = EGLConfAttr 0x302B
egl_MaxPbufferWidth = EGLConfAttr 0x302C
egl_NativeRenderable = EGLConfAttr 0x302D
egl_NativeVisualID = EGLConfAttr 0x302E
egl_NativeVisualType = EGLConfAttr 0x302F
egl_Samples = EGLConfAttr 0x3031
egl_SampleBuffers = EGLConfAttr 0x3032
-- | Bitmask, see below.
egl_SurfaceType = EGLConfAttr 0x3033
-- | 'egl_None' or 'egl_TransparentRgb'.
egl_TransparentType = EGLConfAttr 0x3034
egl_TransparentBlueValue = EGLConfAttr 0x3035
egl_TransparentGreenValue = EGLConfAttr 0x3036
egl_TransparentRedValue = EGLConfAttr 0x3037
egl_BindToTextureRGB = EGLConfAttr 0x3039
egl_BindToTextureRGBA = EGLConfAttr 0x303A
egl_MinSwapInterval = EGLConfAttr 0x303B
egl_MaxSwapInterval = EGLConfAttr 0x303C
egl_LuminanceSize = EGLConfAttr 0x303D
egl_AlphaMaskSize = EGLConfAttr 0x303E
-- | 'egl_RgbBuffer' or 'egl_LuminanceBuffer'.
egl_ColorBufferType = EGLConfAttr 0x303F
-- | Bitmask, see below.
egl_RenderableType = EGLConfAttr 0x3040
egl_MatchNativePixmap = EGLConfAttr 0x3041
egl_Conformant = EGLConfAttr 0x3042

-- *** egl_ConfigCaveat values (except egl_None)
egl_SlowConfig, egl_NonConformantConfig :: EGLint
egl_SlowConfig = 0x3050
egl_NonConformantConfig = 0x3051

-- *** egl_SurfaceType mask bits
egl_PBuffer, egl_Pixmap, egl_Window,
    egl_VGColorspaceLinear, egl_VGAlphaFormatPre,
    egl_MultisampleResolveBox, egl_SwapBehaviorPreserved
    :: EGLint
egl_PBuffer = 0x0001
egl_Pixmap = 0x0002
egl_Window = 0x0004
egl_VGColorspaceLinear = 0x0020
egl_VGAlphaFormatPre = 0x0040
egl_MultisampleResolveBox = 0x0200
egl_SwapBehaviorPreserved = 0x0400

-- *** egl_TransparentType values (except egl_None)
egl_TransparentRgb :: EGLint
egl_TransparentRgb = 0x3052

-- *** egl_ColorBufferType values
egl_RgbBuffer, egl_LuminanceBuffer :: EGLint
egl_RgbBuffer = 0x308E
egl_LuminanceBuffer = 0x308F

-- *** egl_RenderableType bitmask
egl_OpenGLES, egl_OpenVG, egl_OpenGLES2, egl_OpenGL,
	egl_OpenGLES3 :: EGLint
egl_OpenGLES = 1
egl_OpenVG = 2
egl_OpenGLES2 = 4
egl_OpenGL = 8

-- | /EGL_KHR_create_context or EGL 1.5 required/
egl_OpenGLES3 = 0x40


-- * Surface

eglGetSurfaceAttr :: Egl -> EGLSurfAttr -> IO Int32
eglGetSurfaceAttr egl (EGLSurfAttr attr) = do
	EglCurrent{..} <- readIORef egl
	with (-1) $ \ptr -> do
		eglQuerySurface disp dsurf attr ptr
		peek ptr

eglSetSurfaceAttr :: Egl -> EGLSurfAttr -> Int32 -> IO Bool
eglSetSurfaceAttr egl (EGLSurfAttr attr) value = do
	EglCurrent{..} <- readIORef egl
	(/= 0) <$> eglSurfaceAttrib disp dsurf attr value

-- ** Surface Attributes

egl_Height, egl_Width, egl_LargestPbuffer, egl_TextureFormat, egl_TextureTarget,
    egl_MipmapTexture, egl_MipmapLevel, egl_RenderBuffer, egl_VGColorspace,
    egl_VGAlphaFormat, egl_HorizontalResolution, egl_VerticalResolution,
    egl_PixelAspectRaito, egl_SwapBehavior, egl_MultisampleResolve, egl_ConfigId
    :: EGLSurfAttr
-- | int Height of surface.
egl_Height = EGLSurfAttr 0x3056
-- | int Width of surface.
egl_Width = EGLSurfAttr 0x3067
-- | boolean If true, create largest pbuffer possible.
egl_LargestPbuffer = EGLSurfAttr 0x3058
-- | enum Format of texture: RGB, RGBA, or no texture.
egl_TextureFormat = EGLSurfAttr 0x3080
-- | enum Type of texture: 2D or no texture.
egl_TextureTarget = EGLSurfAttr 0x3081
-- | boolean True if texture has mipmaps.
egl_MipmapTexture = EGLSurfAttr 0x3082
-- | int Mipmap level to render to.
egl_MipmapLevel = EGLSurfAttr 0x3083
-- | enum Render buffer, see below.
egl_RenderBuffer = EGLSurfAttr 0x3086
-- | enum Color space for OpenVG, see below.
egl_VGColorspace = EGLSurfAttr 0x3087
-- | enum Alpha format for OpenVG, see below.
egl_VGAlphaFormat = EGLSurfAttr 0x3088
-- | int Horizontal dot pitch.
egl_HorizontalResolution = EGLSurfAttr 0x3090
-- | int Vertical dot pitch.
egl_VerticalResolution = EGLSurfAttr 0x3091
-- | int Display aspect ratio.
egl_PixelAspectRaito = EGLSurfAttr 0x3092
-- | enum Buffer swap behavior, see below.
egl_SwapBehavior = EGLSurfAttr 0x3093
-- | enum Multisample resolve behavior, see below.
egl_MultisampleResolve = EGLSurfAttr 0x3099
-- | int ID of EGLConfig surface was created with egl_Height integer Height of surface.
egl_ConfigId = EGLSurfAttr 0x3028
-- | /EGL_KHR_gl_colorspace or EGL 1.5+/ enum Color space for OpenGL, see below.
egl_GLColorspace = EGLSurfAttr 0x3087

-- *** egl_Texture{Format,Target} values
egl_NoTexture, egl_TextureRgb, egl_TextureRgba, egl_Texture2D :: EGLint
egl_NoTexture = 0x305C
egl_TextureRgb = 0x305D
egl_TextureRgba = 0x305E
egl_Texture2D = 0x305F

-- *** egl_RenderBuffer values
egl_BackBuffer, egl_SingleBuffer :: EGLint
egl_BackBuffer = 0x3084
egl_SingleBuffer = 0x3085

-- *** egl_{VG,GL}Colorspace values
egl_ColorspaceSrgb, egl_ColorspaceLinear :: EGLint
egl_ColorspaceSrgb = 0x3089
egl_ColorspaceLinear = 0x308A

-- *** egl_VGAlphaFormat values
egl_Nonpre, egl_Pre :: EGLint
egl_Nonpre = 0x308B
egl_Pre = 0x308C

-- *** Resolution and Aspect Raito values
egl_DisplayScaling, egl_Unknown :: EGLint
-- | Constant scale factor by which fractional display resolutions &
-- aspect ratio are scaled when queried as integer values.
egl_DisplayScaling = 10000
-- | Unknown display resolution or aspect ratio.
egl_Unknown = -1

-- *** egl_SwapBehavior values
egl_BufferPreserved, egl_BufferDestroyed :: EGLint
egl_BufferPreserved = 0x3094
egl_BufferDestroyed = 0x3095

-- *** egl_MultisampleResolve values
egl_ResolveDefault, egl_ResolveBox :: EGLint
egl_ResolveDefault = 0x309A
egl_ResolveBox = 0x309B


-- * Context

eglConfigID, eglContextClientType, eglContextClientVersion, eglRenderBuffer
    :: Egl -> IO Int32
eglConfigID = queryContext 0x3028
eglContextClientType = queryContext 0x3097
eglContextClientVersion = queryContext 0x3098
eglRenderBuffer = queryContext 0x3086

-- ** Context Attributes

-- | Specify context client major version, 2 or later.
egl_ClientVersion :: EGLContextAttr
egl_ClientVersion = EGLContextAttr 0x3098

-- | /EGL_EXT_create_context_robustness required/
-- Same as unpostfixed version.
egl_GLRobustAccessExt, egl_GLResetNotificationStrategyExt :: EGLContextAttr
egl_GLRobustAccessExt = EGLContextAttr 0x30BF
egl_GLResetNotificationStrategyExt = EGLContextAttr 0x3138

egl_MinorVersion, egl_GLProfileMask,
	egl_GLResetNotificationStrategy :: EGLContextAttr
-- | /EGL_KHR_create_context or EGL 1.5 required/
-- Specify context client minor version.
egl_MinorVersion = EGLContextAttr 0x30FB

-- | /EGL_KHR_create_context or EGL 1.5 required/
-- See below.
egl_GLProfileMask = EGLContextAttr 0x30FD

-- | /EGL_KHR_create_context or EGL 1.5 required/
-- See below.
egl_GLResetNotificationStrategy = EGLContextAttr 0x30BD

-- | /EGL_KHR_create_context required/ See below.
egl_ContextFlags :: EGLContextAttr
egl_ContextFlags = EGLContextAttr 0x30FC

-- | /EGL 1.5 required/ Specify 'egl_True' or 'egl_False'
egl_GLDebug, egl_GLForwardCompat, egl_GLRobustAccess :: EGLContextAttr
egl_GLDebug = EGLContextAttr 0x31B0
egl_GLForwardCompat = EGLContextAttr 0x31B1
egl_GLRobustAccess = EGLContextAttr 0x31B2

-- *** egl_OpenGLProfileMask mask bits
egl_GLCoreProfileBit, egl_GLCompatProfileBit :: EGLint
egl_GLCoreProfileBit = 1
egl_GLCompatProfileBit = 2

-- *** egl_OpenGLResetNotificationStrategy values
egl_NoResetNotification, egl_LoseContextOnReset :: EGLint
egl_NoResetNotification = 0x31BE
egl_LoseContextOnReset = 0x31BF

-- *** egl_ContextFlags bits
egl_GLDebugBit, egl_GLForwardCompatBit, egl_GLRobustAccessBit :: EGLint
egl_GLDebugBit = 1
egl_GLForwardCompatBit = 2
egl_GLRobustAccessBit = 4


-- * Synchronization Primitives

eglWaitCaller :: IO Bool
eglWaitCaller = withErrorCheck "eglWaitClient" eglWaitClient

eglWaitGPU :: IO Bool
eglWaitGPU = withErrorCheck "eglWaitNative" $ eglWaitNative 0x305B
-- EGL_CORE_NATIVE_ENGINE


-- * Copying Surface to Texture

-- | Specify 'egl_RenderBuffer' value. (Perhaps 'egl_BackBuffer' only?)
type EGLBuffer = EGLint

eglBindAsTexture :: Egl -> EGLBuffer -> IO Bool
eglBindAsTexture egl buf = do
	EglCurrent{..} <- readIORef egl
	withErrorCheck "eglBindTexImage" $
		eglBindTexImage disp dsurf buf

eglReleaseTexture :: Egl -> EGLBuffer -> IO Bool
eglReleaseTexture egl buf = do
	EglCurrent{..} <- readIORef egl
	withErrorCheck "eglReleaseTexImage" $
		eglReleaseTexImage disp dsurf buf


-- * Switching between APIs

newtype GraphicsAPI = GraphicsAPI EGLenum

instance Show GraphicsAPI where
	show OpenGLES = "OpenGLES"
	show OpenVG = "OpenVG"
	show OpenGL = "OpenGL"
	show _ = "None"

pattern OpenGLES = GraphicsAPI 0x30A0
pattern OpenVG = GraphicsAPI 0x30A1
pattern OpenGL = GraphicsAPI 0x30A2

eglBindCurrentAPI :: GraphicsAPI -> IO Bool
eglBindCurrentAPI (GraphicsAPI e) =
	withErrorCheck "eglBindAPI" $ eglBindAPI e

eglCurrentAPI :: IO GraphicsAPI
eglCurrentAPI = GraphicsAPI <$> eglQueryAPI

