{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.OpenGLES.Framebuffer (
  -- * Whole Framebuffer Operations
  -- ** Clearing the Buffers
  clear, clearColor, clearDepth, clearStencil,
  BufferMask,
  colorBuffer, depthBuffer, stencilBuffer,
  
  -- ** Fine Control of Buffer Updates
  colorMask, depthMask, stencilMask, stencilMaskSep,
  
  -- * Renderbuffer
  glRenderbuffer,
  unsafeRenderbuffer,
  
  -- * Framebuffer
  glFramebuffer,
  CR(..), Attachable, DepthStencil,
  colorOnly, depthImage, stencilImage, depthStencil,
  
  -- * Framebuffer Settings
  bindFb, withFb,
  defaultFramebuffer,
  viewport, getViewport, withViewport,
  depthRange, getDepthRange, withDepthRange
  ) where
import Control.Applicative
import Data.IORef
import Foreign
import Graphics.OpenGLES.Base
import Graphics.OpenGLES.Caps (hasES3)
import Graphics.OpenGLES.Internal
import Graphics.OpenGLES.PixelFormat
import Graphics.OpenGLES.State
import Graphics.TextureContainer.KTX
import Linear.V2
import Linear.V4


-- |
-- > clear [] colorBuffer
-- > clear [bindFb framebuffer] (colorBuffer+depthBuffer)
clear
	:: [RenderConfig]
	-> BufferMask
	-> GL ()
clear gs (BufferMask flags) = sequence gs >> glClear flags

clearColor :: Float -> Float -> Float -> Float -> GL ()
clearColor = glClearColor

clearDepth :: Float -> GL ()
clearDepth = glClearDepthf

clearStencil :: Int32 -> GL ()
clearStencil = glClearStencil

depthBuffer, stencilBuffer, colorBuffer :: BufferMask
depthBuffer = BufferMask 0x100
stencilBuffer = BufferMask 0x400
colorBuffer = BufferMask 0x4000

colorMask :: V4 Bool -> GL ()
colorMask (V4 r g b a) = glColorMask (f r) (f g) (f b) (f a)
	where f c = if c then 1 else 0

depthMask :: Bool -> GL ()
depthMask = glDepthMask . (\d -> if d then 1 else 0)

stencilMask :: Word32 -> GL ()
stencilMask = glStencilMask

stencilMaskSep :: CullFace -> Word32 -> GL ()
stencilMaskSep (Culling face) = glStencilMaskSeparate face

-- /ES3+/
-- Selecting a Buffer for Writing
-- void DrawBuffers(sizei n, const enum *bufs);
-- bufs points to an array of n BACK, NONE, or COLOR_ATTACHMENTi
-- where i = [0,MAX_COLOR_ATTACHMENTS - 1]
-- void ClearBuffer{if ui}v(enum buffer, int drawbuffer, const T *value);
-- buffer: COLOR, DEPTH, STENCIL
-- void ClearBufferfi(enum buffer, int drawbuffer, float depth, int stencil);
-- buffer: DEPTH_STENCIL. drawbuffer: 0

-- |
-- New Renderbuffer with specified sample count and dimentions.
glRenderbuffer :: forall a b. InternalFormat a b => Int32 -> GL (V2 Int32) -> GL (Renderbuffer b)
glRenderbuffer sample askSize = do
	let (_, _, internalformat) = ifmt ([] :: [(a,b)])
	unsafeRenderbuffer sample askSize internalformat

unsafeRenderbuffer :: Int32 -> GL (V2 Int32) -> GLenum -> GL (Renderbuffer a)
unsafeRenderbuffer samples askSize internalformat = do
	let sample = if hasES3 then samples else 0
	dim <- newIORef undefined
	Renderbuffer samples internalformat dim
		<$> (newGLO glGenRenderbuffers glDeleteRenderbuffers $ \rb -> do
			glBindRenderbuffer 0x8D41 rb
			win@(V2 w h) <- askSize
			writeIORef dim win
			case sample of
				0 -> glRenderbufferStorage 0x8D41 internalformat w h
				_ -> glRenderbufferStorageMultisample 0x8D41 samples internalformat w h
			-- XXX restore content on EGL_CONTEXT_LOST if possible
			)

instance Attachable Renderbuffer a where
	glAttachToFramebuffer attachment (Renderbuffer _ _ dim glo) maxDims = do
		rb <- getObjId glo
		V2 w h <- readIORef dim
		modifyIORef maxDims (\(V2 mw mh)-> V2 (max w mw) (max h mh))
		glFramebufferRenderbuffer 0x8D40 attachment 0x8D41 rb

instance Attachable Texture a where
	glAttachToFramebuffer attachment (Texture textgt ktx glo) maxDims = do
		tex <- getObjId glo
		k <- readIORef ktx
		let w = fromIntegral $ ktxPixelWidth k
		let h = fromIntegral $ ktxPixelHeight k
		modifyIORef maxDims (\(V2 mw mh)-> V2 (max w mw) (max h mh))
		let level = 0; layer = 0
		-- XXX multisample texure sholud use texture_2d_multisample
		case textgt of
			x | x == texture_2d ->
				glFramebufferTexture2D 0x8D40 attachment textgt tex level
			x | x == texture_cube_map ->
				glFramebufferTexture2D 0x8D40 attachment texture_cube_map_positive_x tex level
			x | x == texture_2d_array ->
				glFramebufferTextureLayer textgt attachment tex level layer
			x | x == texture_3d ->
				glFramebufferTextureLayer textgt attachment tex level layer
			_ -> error "glAttachToFramebuffer: Invalid Texture target"

data CR = forall a c. (Attachable a c, ColorRenderable c) => CR (a c)
newtype DepthStencil = DepthStencil (IORef (V2 Int32) -> GL ())

colorOnly :: DepthStencil
colorOnly = DepthStencil (const $ return ())

depthImage :: (Attachable a d, DepthRenderable d) => a d -> DepthStencil
depthImage = DepthStencil . glAttachToFramebuffer 0x8D00

stencilImage :: (Attachable a s, StencilRenderable s) => a s -> DepthStencil
stencilImage = DepthStencil . glAttachToFramebuffer 0x8D20

depthStencil :: (Attachable a r, DepthRenderable r, StencilRenderable r) => a r -> DepthStencil
depthStencil = DepthStencil . glAttachToFramebuffer 0x821A

-- | New 'Framebuffer' from specified 'ColorRenderable' and 'DepthStencil'
glFramebuffer :: [CR] -> DepthStencil -> GL Framebuffer
glFramebuffer colours (DepthStencil runds) = do
	maxDims <- newIORef (V2 0 0)
	Framebuffer maxDims
		<$> (newGLO glGenFramebuffers glDeleteFramebuffers $ \fb -> do
			glBindFramebuffer 0x8D40 fb
			sequence_ $ zipWith (go maxDims) [0x8CE0..] colours
			runds maxDims
			)
	where go maxDims attachment (CR cr) =
		glAttachToFramebuffer attachment cr maxDims

bindFb :: Framebuffer -> GL ()
bindFb (Framebuffer _ glo) =
	getObjId glo >>= glBindFramebuffer 0x8D40

-- XXX maybe slow (untested)
withFb :: Framebuffer -> GL a -> GL a
withFb fb io = do
	-- GL_FRAMEBUFFER_BINDING
	fb' <- alloca $ \p -> glGetIntegerv 0x8CA6 p >> peek p
	bindFb fb
	result <- io
	glBindFramebuffer 0x8D40 (fromIntegral fb')
	return result

-- XXX maybe slow (untested)
getViewport :: GL (V4 Int32)
getViewport = allocaArray 4 $ \p -> do
	glGetIntegerv 0x0BA2 p -- GL_VIEWPORT
	[x,y,w,h] <- peekArray 4 p
	return $ V4 x y w h

-- |
-- Cliping current framebuffer. Note that origin is left-bottom.
viewport :: V4 Int32 -> GL ()
viewport (V4 x y w h) = glViewport x y w h

withViewport :: V4 Int32 -> GL a -> GL a
withViewport vp io = do
	old <- getViewport
	viewport vp
	result <- io
	viewport old
	return result

-- XXX maybe slow (untested)
getDepthRange :: GL (V2 Float)
getDepthRange = allocaArray 2 $ \p -> do
	glGetFloatv 0x0B70 p -- GL_DEPTH_RANGE
	[n,f] <- peekArray 2 p
	return $ V2 n f

depthRange :: V2 Float -> GL ()
depthRange (V2 near far) = glDepthRangef near far

withDepthRange :: V2 Float -> GL a -> GL a
withDepthRange dr io = do
	old <- getDepthRange
	depthRange dr
	result <- io
	depthRange old
	return result

