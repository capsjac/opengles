{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Graphics.OpenGLES.PixelFormat where
import Data.Proxy
import GHC.TypeLits
import Data.Int
import Data.Word
import Graphics.OpenGLES.Internal
import Graphics.OpenGLES.Base
import Linear.Vect
--import Linear

-- Texture Only Sized Internal Formats
data R8snorm
data Rg8snorm
data Rgb8snorm
data Rgba8snorm
data Srgb8
data R16f
data Rg16f
data Rgb16f
data Rgba16f
data R32f
data Rg32f
data Rgb32f
data Rgba32f
data B10fG11fR11f
data E5bgr9
data Rgb8i
data Rgb8ui
data Rgb16i
data Rgb16ui
data Rgb32i
data Rgb32ui

-- Color Renderable Sized Internal Formats
data R8
data Rg8
data Rgb8
data Rgb565
data Rgba4
data Rgb5a1
data Rgba8
data A2bgr10
data A2bgr10ui
data Srgb8a8
data R8i
data R8ui
data R16i
data R16ui
data R32i
data R32ui
data Rg8i
data Rg8ui
data Rg16i
data Rg16ui
data Rg32i
data Rg32ui
data Rgba8i
data Rgba8ui
data Rgba16i
data Rgba16ui
data Rgba32i
data Rgba32ui

-- Depth/Stencil Renderable Sized Internal Formats
data Depth16
data Depth24
data Depth32f
data Depth24Stencil8
data Depth32fStencil8
data Stencil8 -- optional

-- Effective internal formats which do not correspond to GL constants
data Luminance8Alpha8
data Luminance8
data Alpha8


class InternalFormat a b | b -> a where
	ifmt :: p (a, b) -> (GLenum,GLenum,GLenum)

class ExternalFormat a b where
	efmt :: p (a, b) -> (GLenum,GLenum,GLenum)

class InternalFormat a b => ES2Format a b where
	es2fmt :: p (a, b) -> (GLenum,GLenum,GLenum)
	es2fmt x = case ifmt x of
		(format, typ, _) -> (format, typ, format)

#define PixelFormat(_dim, _type, _tag, _format, _internal_fmt) \
instance InternalFormat (_dim _type) _tag where \
	{-# INLINE ifmt #-}; \
	ifmt _ = (_format,glType ([] :: [_type]),_internal_fmt) \

PixelFormat(, Int8, R8snorm, r, 0x8F94)
PixelFormat(V2, Int8, Rg8snorm, rg, 0x8F95)
PixelFormat(V3, Int8, Rgb8snorm, rgb, 0x8F96)
PixelFormat(V4, Int8, Rgba8snorm, rgba, 0x8F97)
PixelFormat(V3, Word8, Srgb8, rgb, 0x8C41)
PixelFormat(, HalfFloat, R16f, r, 0x822D)
PixelFormat(V2, HalfFloat, Rg16f, rg, 0x822F)
PixelFormat(V3, HalfFloat, Rgb16f, rgb, 0x881B)
PixelFormat(V4, HalfFloat, Rgba16f, rgba, 0x881A)
PixelFormat(, Float, R32f, r, 0x822E)
PixelFormat(V2, Float, Rg32f, rg, 0x8230)
PixelFormat(V3, Float, Rgb32f, rgb, 0x8815)
PixelFormat(V4, Float, Rgba32f, rgba, 0x8814)
PixelFormat(, Word10f11f11f, B10fG11fR11f, rgb, 0x8C3A)
PixelFormat(, Word5999, E5bgr9, rgb, 0x8C3D)
PixelFormat(V3, Int8, Rgb8i, rgb_integer, 0x8D8F)
PixelFormat(V3, Word8, Rgb8ui, rgb_integer, 0x8D7D)
PixelFormat(V3, Int16, Rgb16i, rgb_integer, 0x8D89)
PixelFormat(V3, Word16, Rgb16ui, rgb_integer, 0x8D77)
PixelFormat(V3, Int32, Rgb32i, rgb_integer, 0x8D83)
PixelFormat(V3, Word32, Rgb32ui, rgb_integer, 0x8D71)

PixelFormat(, Word8, R8, r, 0x8229)
PixelFormat(V2, Word8, Rg8, rg, 0x822B)
PixelFormat(V3, Word8, Rgb8, rgb, 0x8051)
PixelFormat(, Word565, Rgb565, rgb, 0x8D62)
PixelFormat(, Word4444, Rgba4, rgba, 0x8056)
PixelFormat(, Word5551, Rgb5a1, rgba, 0x8057)
PixelFormat(V4, Word8, Rgba8, rgba, 0x8058)
PixelFormat(, Word2_10x3, A2bgr10, rgba, 0x8059)
PixelFormat(, Word2_10x3, A2bgr10ui, rgba_integer, 0x906F)
PixelFormat(V4, Word8, Srgb8a8, rgba, 0x8C43)
PixelFormat(, Int8, R8i, r_integer, 0x8231)
PixelFormat(, Word8, R8ui, r_integer, 0x8232)
PixelFormat(, Int16, R16i, r_integer, 0x8233)
PixelFormat(, Word16, R16ui, r_integer, 0x8234)
PixelFormat(, Int32, R32i, r_integer, 0x8235)
PixelFormat(, Word32, R32ui, r_integer, 0x8236)
PixelFormat(V2, Int8, Rg8i, rg_integer, 0x8237)
PixelFormat(V2, Word8, Rg8ui, rg_integer, 0x8238)
PixelFormat(V2, Int16, Rg16i, rg_integer, 0x8239)
PixelFormat(V2, Word16, Rg16ui, rg_integer, 0x823A)
PixelFormat(V2, Int32, Rg32i, rg_integer, 0x823B)
PixelFormat(V2, Word32, Rg32ui, rg_integer, 0x823C)
PixelFormat(V4, Int8, Rgba8i, rgba_integer, 0x8D8E)
PixelFormat(V4, Word8, Rgba8ui, rgba_integer, 0x8D7C)
PixelFormat(V4, Int16, Rgba16i, rgba_integer, 0x8D88)
PixelFormat(V4, Word16, Rgba16ui, rgba_integer, 0x8D76)
PixelFormat(V4, Int32, Rgba32i, rgba_integer, 0x8D82)
PixelFormat(V4, Word32, Rgba32ui, rgba_integer, 0x8D70)

PixelFormat(, Word16, Depth16, depth_component, 0x81A5)
PixelFormat(, Word32, Depth24, depth_component, 0x81A6)
PixelFormat(, Float, Depth32f, depth_component, 0x8CAC)
PixelFormat(, Word24_8, Depth24Stencil8, depth_stencil, 0x88F0)
PixelFormat(, FloatWord24_8, Depth32fStencil8, depth_stencil, 0x8CAD)
--PixelFormat(, Word8, StencilIx8, 0x8D48, 0x1901) -- /GL_OES_texture_stencil8/
PixelFormat(V2, Word8, Luminance8Alpha8, 0x190A, 0x190A)
PixelFormat(, Word8, Luminance8, 0x1909, 0x1909)
PixelFormat(, Word8, Alpha8, 0x1906, 0x1906)

instance InternalFormat a b => ExternalFormat a b where
	{-# INLINE efmt #-}
	efmt = ifmt

#define PixelFormat2(_dim, _type, _tag, _format, _internal_fmt) \
instance ExternalFormat (_dim _type) _tag where \
	{-# INLINE efmt #-}; \
	efmt _ = (_format,glType ([] :: [_type]),_internal_fmt) \

PixelFormat2(V4, Word8, Rgb5a1, rgba, 0x8057) -- RGBA W8
PixelFormat2(V4, Word8, Rgba4, rgba, 0x8056) -- RGBA W8
PixelFormat2(, Word2_10x3, Rgb5a1, rgba, 0x8057) -- ABGR 2_10_10_10
PixelFormat2(V4, Float, Rgba16f, rgba, 0x881A) -- RGBA Float
PixelFormat2(V3, Word8, Rgb565, rgb, 0x8D62) -- RGB W8
PixelFormat2(V3, HalfFloat, B10fG11fR11f, rgb, 0x8C3A) -- RGB HalfFloat
PixelFormat2(V3, HalfFloat, E5bgr9, rgb, 0x8C3D) -- RGB HalfFloat
PixelFormat2(V3, Float, B10fG11fR11f, rgb, 0x8C3A) -- RGB Float
PixelFormat2(V3, Float, E5bgr9, rgb, 0x8C3D) -- RGB Float
PixelFormat2(V2, Float, Rg16f, rg, 0x822F) -- RG Float
PixelFormat2(, Float, R16f, r, 0x822D) -- R Float
PixelFormat2(, Word32, Depth16, depth_component, 0x81A5) -- D W32

instance ES2Format (V4 Word8) Rgba8 -- (RGBA,UByte)
instance ES2Format Word4444 Rgba4 -- (RGBA,UShort4444)
instance ES2Format Word5551 Rgb5a1 -- (RGBA,UShort5551)
instance ES2Format (V3 Word8) Rgb8 -- (RGB,UByte)
instance ES2Format Word565 Rgb565 -- (RGB,UShort565)
instance ES2Format (V2 Word8) Luminance8Alpha8 -- (LUMINANCE_ALPHA,UByte)
instance ES2Format Word8 Luminance8 -- (LUMINANCE,UByte)
instance ES2Format Word8 Alpha8 -- (ALPHA,UByte)


class ColorRenderable a where
instance ColorRenderable R8
instance ColorRenderable Rg8
instance ColorRenderable Rgb8
instance ColorRenderable Rgb565
instance ColorRenderable Rgba4
instance ColorRenderable Rgb5a1
instance ColorRenderable Rgba8
instance ColorRenderable A2bgr10
instance ColorRenderable A2bgr10ui
instance ColorRenderable Srgb8a8
instance ColorRenderable R8i
instance ColorRenderable R8ui
instance ColorRenderable R16i
instance ColorRenderable R16ui
instance ColorRenderable R32i
instance ColorRenderable R32ui
instance ColorRenderable Rg8i
instance ColorRenderable Rg8ui
instance ColorRenderable Rg16i
instance ColorRenderable Rg16ui
instance ColorRenderable Rg32i
instance ColorRenderable Rg32ui
instance ColorRenderable Rgba8i
instance ColorRenderable Rgba8ui
instance ColorRenderable Rgba16i
instance ColorRenderable Rgba16ui
instance ColorRenderable Rgba32i
instance ColorRenderable Rgba32ui

class DepthRenderable a where
instance DepthRenderable Depth16
instance DepthRenderable Depth24
instance DepthRenderable Depth32f
instance DepthRenderable Depth24Stencil8
instance DepthRenderable Depth32fStencil8

class StencilRenderable a where
instance StencilRenderable Depth24Stencil8
instance StencilRenderable Depth32fStencil8
instance StencilRenderable Stencil8 -- optional

type family SizeOf (f :: *) :: Nat
type instance SizeOf Float = 4
type instance SizeOf HalfFloat = 2
--type instance SizeOf FixedFloat = 4
type instance SizeOf Word8 = 1
type instance SizeOf Word16 = 2
type instance SizeOf Word32 = 4
type instance SizeOf Int8 = 1
type instance SizeOf Int16 = 2
type instance SizeOf Int32 = 4
type instance SizeOf Int2_10x3 = 4
type instance SizeOf Word2_10x3 = 4
type instance SizeOf Word4444 = 2
type instance SizeOf Word5551 = 2
type instance SizeOf Word10f11f11f = 4
type instance SizeOf Word5999 = 4
type instance SizeOf Word24_8 = 4
type instance SizeOf FloatWord24_8 = 8
type instance SizeOf (V2 a) = Aligned (2 * SizeOf a)
type instance SizeOf (V3 a) = Aligned (3 * SizeOf a)
type instance SizeOf (V4 a) = Aligned (4 * SizeOf a)

--type family Sum (l :: [Nat]) :: Nat
--type instance Sum '[] = 0
--type instance Sum (x ': xs) = x + Sum xs

--type family Map (f :: * -> *) (xs :: [*]) :: [*]
--type instance Map f '[] = '[]
--type instance Map f (x ': xs) = f x ': Map f xs

type family Stride (l :: [*]) :: Nat
type instance Stride '[] = 0
type instance Stride (x ': xs) = SizeOf x + Stride xs

type family Aligned (x :: Nat) :: Nat
type instance Aligned x = If (x <=? 3) 4 x

type family If (p :: Bool) (t :: Nat) (f :: Nat) :: Nat
type instance If True x y = x
type instance If False x y = y

--type family ImageOf
--type instance ImageOf Rgba8 = V4 Word8
--type instance ImageOf Rgba4 = Word16
--type instance ImageOf Rgba4FromRgba8 = Word16

