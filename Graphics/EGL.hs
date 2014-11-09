{-# LANGUAGE RecordWildCards #-}
-- | EGL 1.4, carefully reconstructed.
-- You can still use the rest of C APIs via "Graphics.EGL.Base".
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

-- | 
eglInit :: IO Egl
eglInit = eglInitializeOn Nothing
	[[(egl_RenderableType, egl_OpenGLES2)
	 ,(egl_SurfaceType, egl_Window)
	 ,(egl_BlueSize, 8)
	 ,(egl_GreenSize, 8)
	 ,(egl_RedSize, 8)
	 ,(egl_DepthSize, 24) ]
	,[(egl_RenderableType, egl_OpenGLES2)
	 ,(egl_SurfaceType, egl_Window)
	 ,(egl_BlueSize, 8)
	 ,(egl_GreenSize, 8)
	 ,(egl_RedSize, 8)
	 ,(egl_DepthSize, 16) ]]
	[(egl_ContextClientVersion, 2)]

-- | 
eglInitializeOn :: Maybe EGLNativeDisplay -> [[(EGLConfAttr, Int32)]] -> [(EGLContextAttr, Int32)] -> IO Egl
eglInitializeOn nd cfgs = newIORef . initial nd cfgs

-- | 
eglResume :: Egl -> EGLNativeWindow -> IO ()
eglResume egl window = do
	glLog "eglResume..."
	cur@EglCurrent{..} <- readIORef egl
	let share = nullPtr
	if disp == nullPtr then do -- context not initialized
		(display, config) <- setupEgl ndisp confcand
		(surf, w, h) <- setSurface display config window
		cxt <- withAttrList (map (\(EGLContextAttr a,b)->(a,b))cxtconf) $
			eglCreateContext display config share
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
		(surf, w, h) <- setSurface disp chosen window
		when (screenDims /= (w, h)) $
			glLog "screen resized"
		res <- eglMakeCurrent disp surf surf context
		when (res == 0) $ do -- EGL_FALSE
			err <- eglGetError
			glLog $ "eglResume: " ++ showEglError err
			if err == 0x300E then do -- EGL_CONTEXT_LOST
				glLog "Re-creating EGL context..."
				cxt <- withAttrList (map (\(EGLContextAttr a,b)->(a,b))cxtconf) $
					eglCreateContext disp chosen share
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

-- | 
eglSuspend :: Egl -> IO ()
eglSuspend egl = do
	glLog "eglSuspend..."
	c@EglCurrent{..} <- readIORef egl
	when (dsurf /= nullPtr) $ do
		eglDestroySurface disp dsurf
		writeIORef egl c { dsurf = nullPtr }

-- | 
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

--eglShareContext :: Egl -> IO Egl


-- * Posting the Color Buffer

-- | Post EGL surface color buffer to a native window.
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

eglConfigID, eglContextClientType, eglContextClientVersion, eglRenderBuffer
    :: Egl -> IO Int32
eglConfigID = queryContext 0x3028
eglContextClientType = queryContext 0x3097
eglContextClientVersion = queryContext 0x3098
eglRenderBuffer = queryContext 0x3086

eglScreenDims :: Num a => Egl -> IO (a, a)
eglScreenDims egl = do
	(w, h) <- screenDims <$> readIORef egl
	return (fromIntegral w, fromIntegral h)

-- * Attributes

egl_True, egl_False, egl_None :: EGLint
egl_True = 1
egl_False = 0
egl_None = 0x3038


-- ** EGLConfig Attributes

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
egl_SurfaceType = EGLConfAttr 0x3033
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
egl_ColorBufferType = EGLConfAttr 0x303F
egl_RenderableType = EGLConfAttr 0x3040
egl_MatchNativePixmap = EGLConfAttr 0x3041
egl_Conformant = EGLConfAttr 0x3042

-- | EGL_CONFIG_CAVEAT value EGL_NONE | 0x3050 | 0x3051
egl_SlowConfig, egl_NonConformantConfig :: EGLint
egl_SlowConfig = 0x3050
egl_NonConformantConfig = 0x3051

-- | EGL_TRANSPARENT_TYPE value EGL_NONE | 0x3052
egl_TransparentRgb :: EGLint
egl_TransparentRgb = 0x3052

-- | EGL_COLOR_BUFFER_TYPE value
egl_RgbBuffer, egl_LuminanceBuffer :: EGLint
egl_RgbBuffer = 0x308E
egl_LuminanceBuffer = 0x308F

-- | EGL_TEXTURE_FORMAT value
egl_NoTexture, egl_TextureRgb, egl_TextureRgba, egl_Texture2D :: EGLint
egl_NoTexture = 0x305C
egl_TextureRgb = 0x305D
egl_TextureRgba = 0x305E
egl_Texture2D = 0x305F

-- | EGL_SURFACE_TYPE mask bits
egl_PBuffer, egl_Pixmap, egl_Window,
    egl_VGColorspaceLinear, egl_VGAlphaFormatPre,
    egl_MultisampleResolveBox, egl_SwapBehaviorPreserved
    :: EGLint
egl_PBuffer = 1
egl_Pixmap = 2
egl_Window = 4
egl_VGColorspaceLinear = 0x20
egl_VGAlphaFormatPre = 0x40
egl_MultisampleResolveBox = 0x200
egl_SwapBehaviorPreserved = 0x400

-- | EGL_RENDERABLE_TYPE bitmask
egl_OpenGLES, egl_OpenVG, egl_OpenGLES2, egl_OpenGL :: EGLint
egl_OpenGLES = 1
egl_OpenVG = 2
egl_OpenGLES2 = 4
egl_OpenGL = 8


-- ** EGLSurface Attributes

egl_Height, egl_Width, egl_LargestPbuffer, egl_TextureFormat, egl_TextureTarget,
    egl_MipmapTexture, egl_MipmapLevel, egl_RenderBuffer, egl_VGColorspace,
    egl_VGAlphaFormat, egl_HorizontalResolution, egl_VerticalResolution,
    egl_PixelAspectRaito, egl_SwapBehavior, egl_MultisampleResolve, egl_ConfigId
    :: EGLSurfAttr
-- | int Height of surface
egl_Height = EGLSurfAttr 0x3056
-- | int Width of surface
egl_Width = EGLSurfAttr 0x3067
-- | boolean If true, create largest pbuffer possible
egl_LargestPbuffer = EGLSurfAttr 0x3058
-- | enum Format of texture: RGB, RGBA, or no texture
egl_TextureFormat = EGLSurfAttr 0x3080
-- | enum Type of texture: 2D or no texture
egl_TextureTarget = EGLSurfAttr 0x3081
-- | boolean True if texture has mipmaps
egl_MipmapTexture = EGLSurfAttr 0x3082
-- | int Mipmap level to render to
egl_MipmapLevel = EGLSurfAttr 0x3083
-- | enum Render buffer
egl_RenderBuffer = EGLSurfAttr 0x3086
-- | enum Color space for OpenVG
egl_VGColorspace = EGLSurfAttr 0x3087
-- | enum Alpha format for OpenVG
egl_VGAlphaFormat = EGLSurfAttr 0x3088
-- | int Horizontal dot pitch
egl_HorizontalResolution = EGLSurfAttr 0x3090
-- | int Vertical dot pitch
egl_VerticalResolution = EGLSurfAttr 0x3091
-- | int Display aspect ratio
egl_PixelAspectRaito = EGLSurfAttr 0x3092
-- | enum Buffer swap behavior
egl_SwapBehavior = EGLSurfAttr 0x3093
-- | enum Multisample resolve behavior
egl_MultisampleResolve = EGLSurfAttr 0x3099
-- | int ID of EGLConfig surface was created with EGL_HEIGHT integer Height of surface
egl_ConfigId = EGLSurfAttr 0x3028


-- ** EGLContext Attributes

-- | specify 2 or later
egl_ContextClientVersion :: EGLContextAttr
egl_ContextClientVersion = EGLContextAttr 0x3098


--  * Synchronization Primitives

--eglWaitClient :: IO EGLError
--eglWaitClient = toEglErr c_eglWaitClient

--eglWaitNative :: IO EGLError
--eglWaitNative = toEglErr (c_eglWaitNative 0x305B)


--  * Render to Textures

-- EGL_RENDER_BUFFER value 0x3084 | 0x3085
-- back buffer only?
--data EGLBuffer = EGLBackBuffer | EGLSingleBuffer deriving Eq
--eglBindTexImage :: EGLDisplay -> EGLSurface -> EGLBuffer -> IO EGLError
--eglBindTexImage display surface buffer =
--  toEglErr (c_eglBindTexImage (unD display) (unS surface) (if buffer == EGLBackBuffer then 0x3084 else 0x3085))

--eglReleaseTexImage :: EGLDisplay -> EGLSurface -> EGLBuffer -> IO EGLError
--eglReleaseTexImage display surface buffer =
--  toEglErr (c_eglReleaseTexImage (unD display) (unS surface) (if buffer == EGLBackBuffer then 0x3084 else 0x3085))

