{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Graphics.OpenGLES.Types (

  -- * Shading Language Base Types
  M23, M24, M32, M34, M42,
  Vec2, Vec3, Vec4,
  -- BVec2, BVec3, BVec4,
  IVec2, IVec3, IVec4,
  UVec2, UVec3, UVec4,
  Mat2, Mat3, Mat4,
  Mat2x3, Mat2x4, Mat3x2,
  Mat3x4, Mat4x2, Mat4x3,
  
  -- * Vertex Attribute Array Source Datatypes
  HalfFloat(..), FixedFloat(..),
  Int2_10x3(..), Word2_10x3(..),

  -- * Texture Pixel Formats
  Word4444(..), Word5551(..), Word565(..),
  Word10f11f11f(..), Word5999(..), Word24_8(..),
  FloatWord24_8(..),

  -- * Type-Level Utilities
  SizeOf, Aligned, Stride, castGL,

  -- * Uniform Variable
  Uniform, UnifVal,
  
  -- * Vertex Attribute
  Attrib, VertexAttribute,
  AttrElement, Vectorize, VDim,
  VertexAttributeArray, AttrStruct,

  GLStorable(..)
  ) where

import Control.Applicative
import Control.Lens.Indexed (FoldableWithIndex, iforM_)
import Control.Lens.Getter ((^.))
import Control.Monad (when)
import Data.Distributive
import Data.Proxy
import Foreign
import GHC.TypeLits
import Graphics.OpenGLES.Base
import Graphics.OpenGLES.Internal
import Linear
import Unsafe.Coerce

type M23 a = V2 (V3 a)
type M24 a = V2 (V4 a)
type M32 a = V3 (V2 a)
type M34 a = V3 (V4 a)
type M42 a = V4 (V2 a)
type Vec2 = V2 Float
type Vec3 = V3 Float
type Vec4 = V4 Float
--type BVec2 = V2 Bool
--type BVec3 = V3 Bool
--type BVec4 = V4 Bool
type IVec2 = V2 Int32
type IVec3 = V3 Int32
type IVec4 = V4 Int32
type UVec2 = V2 Word32
type UVec3 = V3 Word32
type UVec4 = V4 Word32
type Mat2 = M22 Float
type Mat3 = M33 Float
type Mat4 = M44 Float
type Mat2x3 = M23 Float
type Mat2x4 = M24 Float
type Mat3x2 = M32 Float
type Mat3x4 = M34 Float
type Mat4x2 = M42 Float
type Mat4x3 = M43 Float

type family SizeOf (f :: *) :: Nat where
	SizeOf Float = 4
	SizeOf HalfFloat = 2
	SizeOf FixedFloat = 4
	SizeOf Word8 = 1
	SizeOf Word16 = 2
	SizeOf Word32 = 4
	SizeOf Int8 = 1
	SizeOf Int16 = 2
	SizeOf Int32 = 4
	SizeOf Int2_10x3 = 4
	SizeOf Word2_10x3 = 4
	SizeOf Word4444 = 2
	SizeOf Word5551 = 2
	SizeOf Word10f11f11f = 4
	SizeOf Word5999 = 4
	SizeOf Word24_8 = 4
	SizeOf FloatWord24_8 = 8
	SizeOf (V2 a) = 2 * SizeOf a
	SizeOf (V3 a) = 3 * SizeOf a
	SizeOf (V4 a) = 4 * SizeOf a
	SizeOf Double = 8 -- OpenGL

type family Aligned (x :: Nat) :: Nat where
	Aligned 0 = 0
	Aligned 1 = 4
	Aligned 2 = 4
	Aligned 3 = 4
	Aligned x = 4 + Aligned (x - 4)

type family Stride (list :: [*]) :: Nat where
	Stride '[] = 0
	Stride (x ': xs) = Aligned (SizeOf x) + Stride xs

castGL ::
	CmpNat (Aligned (SizeOf x)) (Aligned (SizeOf y)) ~ EQ
	=> p x -> p y
castGL = unsafeCoerce

--type family If (p :: Bool) (t :: Nat) (f :: Nat) :: Nat where
--	If True x y = x
--	If False x y = y

--type family Sum (l :: [Nat]) :: Nat
--type instance Sum '[] = 0
--type instance Sum (x ': xs) = x + Sum xs

--type family Map (f :: * -> *) (xs :: [*]) :: [*]
--type instance Map f '[] = '[]
--type instance Map f (x ': xs) = f x ': Map f xs

--type family ImageOf
--type instance ImageOf Rgba8 = V4 Word8
--type instance ImageOf Rgba4 = Word16
--type instance ImageOf Rgba4FromRgba8 = Word16


--instance UnifVal Float where
--	glUniform (loc, _, _) x = glUniform1f loc x

#define Uniform(_typ, _arg, _suffix, _rhs) \
instance UnifVal (_typ) where \
	glUniform (loc, _, _) _arg = glUniform/**/_suffix loc _rhs \

Uniform(Float,x,1f,x)
Uniform(Vec2,(V2 x y),2f,x y)
Uniform(Vec3,(V3 x y z),3f,x y z)
Uniform(Vec4,(V4 x y z w),4f,x y z w)
Uniform(Int32,x,1i,x)
Uniform(IVec2,(V2 x y),2i,x y)
Uniform(IVec3,(V3 x y z),3i,x y z)
Uniform(IVec4,(V4 x y z w),4i,x y z w)
Uniform(Word32,x,1ui,x)
Uniform(UVec2,(V2 x y),2ui,x y)
Uniform(UVec3,(V3 x y z),3ui,x y z)
Uniform(UVec4,(V4 x y z w),4ui,x y z w)

{-# NOINLINE pokeUniformArray #-}
pokeUniformArray
	:: Storable b => (GLint -> GLsizei -> Ptr a -> GL ())
	-> (GLint, GLsizei, Ptr ()) -> [b] -> GL ()
pokeUniformArray glUniformV (loc, len, ptr) values = do
	pokeArray (castPtr ptr) (take (fromIntegral len) values)
	glUniformV loc len (castPtr ptr)

instance UnifVal [Float] where glUniform = pokeUniformArray glUniform1fv
instance UnifVal [Vec2] where glUniform = pokeUniformArray glUniform2fv
instance UnifVal [Vec3] where glUniform = pokeUniformArray glUniform3fv
instance UnifVal [Vec4] where glUniform = pokeUniformArray glUniform4fv
instance UnifVal [Int32] where glUniform = pokeUniformArray glUniform1iv
instance UnifVal [IVec2] where glUniform = pokeUniformArray glUniform2iv
instance UnifVal [IVec3] where glUniform = pokeUniformArray glUniform3iv
instance UnifVal [IVec4] where glUniform = pokeUniformArray glUniform4iv
instance UnifVal [Word32] where glUniform = pokeUniformArray glUniform1uiv
instance UnifVal [UVec2] where glUniform = pokeUniformArray glUniform2uiv
instance UnifVal [UVec3] where glUniform = pokeUniformArray glUniform3uiv
instance UnifVal [UVec4] where glUniform = pokeUniformArray glUniform4uiv

castMat a b c d e = a b c d (castPtr e)

instance UnifMat Mat2 where glUnifMat = castMat glUniformMatrix2fv
instance UnifMat Mat3 where glUnifMat = castMat glUniformMatrix3fv
instance UnifMat Mat4 where glUnifMat = castMat glUniformMatrix4fv
-- /GL_NV_non_square_matrices/ seems ignorable for now.
-- http://delphigl.de/glcapsviewer/gles_extensions.php
instance UnifMat Mat2x3 where glUnifMat = castMat glUniformMatrix2x3fv
instance UnifMat Mat2x4 where glUnifMat = castMat glUniformMatrix2x4fv
instance UnifMat Mat3x2 where glUnifMat = castMat glUniformMatrix3x2fv
instance UnifMat Mat3x4 where glUnifMat = castMat glUniformMatrix3x4fv
instance UnifMat Mat4x2 where glUnifMat = castMat glUniformMatrix4x2fv
instance UnifMat Mat4x3 where glUnifMat = castMat glUniformMatrix4x3fv

-- | Matrix __Not tested!!!__
instance (Distributive g, Functor f, UnifMat (f (g a)), Storable (g (f a))) =>
		UnifVal (f (g a)) where
		glUniform (loc, _, ptr) val = glUniform (loc, 1, ptr) [val]
-- | Array of matrix __Not tested!!!__
instance (Distributive g, Functor f, UnifMat (f (g a)), Storable (g (f a))) =>
		UnifVal [f (g a)] where
	-- 'transpose' argument must be GL_FALSE in GL ES 2.0.
	-- GL ES 3.0+ supports transpose.
	glUniform (loc, len, ptr) matrices = do
		pokeArray (castPtr ptr) $ map distribute $ take (fromIntegral len) matrices
		glUnifMat loc len 0 (castPtr ptr :: Ptr (f (g a)))


-- Arrays of attribute is not allowed in GLSL ES

instance VertexAttribute Float where
	glVertexAttrib ix x = glVertexAttrib1f ix x
instance VertexAttribute Vec2 where
	glVertexAttrib ix (V2 x y) = glVertexAttrib2f ix x y
instance VertexAttribute Vec3 where
	glVertexAttrib ix (V3 x y z) = glVertexAttrib3f ix x y z
instance VertexAttribute Vec4 where
	glVertexAttrib ix (V4 x y z w) = glVertexAttrib4f ix x y z w
instance VertexAttribute Int32 where
	glVertexAttrib ix x = glVertexAttribI4i ix x 0 0 1
instance VertexAttribute IVec2 where
	glVertexAttrib ix (V2 x y) = glVertexAttribI4i ix x y 0 1
instance VertexAttribute IVec3 where
	glVertexAttrib ix (V3 x y z) = glVertexAttribI4i ix x y z 1
instance VertexAttribute IVec4 where
	glVertexAttrib ix (V4 x y z w) = glVertexAttribI4i ix x y z w
instance VertexAttribute Word32 where
	glVertexAttrib ix x = glVertexAttribI4ui ix x 0 0 1
instance VertexAttribute UVec2 where
	glVertexAttrib ix (V2 x y) = glVertexAttribI4ui ix x y 0 1
instance VertexAttribute UVec3 where
	glVertexAttrib ix (V3 x y z) = glVertexAttribI4ui ix x y z 1
instance VertexAttribute UVec4 where
	glVertexAttrib ix (V4 x y z w) = glVertexAttribI4ui ix x y z w
instance VertexAttribute a => VertexAttribute (V1 a) where
	glVertexAttrib ix (V1 x) = glVertexAttrib ix x

-- | Matrices __Not tested!!!__
instance (Functor f, Floating a, Distributive g, VertexAttribute (f a),
		FoldableWithIndex (E V4) g) => VertexAttribute (f (g a)) where
	glVertexAttrib ix m = iforM_ (distribute m) $
		\(E i) v -> do
			let index = ix + (V4 0 1 2 3)^.i
			glDisableVertexAttribArray index
			glVertexAttrib index v

instance AttrElement Word8
instance AttrElement Word16
instance AttrElement Word32
instance AttrElement Int8
instance AttrElement Int16
instance AttrElement Int32
instance AttrElement Float
instance AttrElement HalfFloat
instance AttrElement FixedFloat
instance AttrElement Int2_10x3
instance AttrElement Word2_10x3

-- | Temporarily gives a vector representation for type comparison
type family Vectorize a :: * where
	Vectorize Int2_10x3 = V4 Int2_10x3
	Vectorize Word2_10x3 = V4 Word2_10x3
	Vectorize (f Int2_10x3) = f (V4 Int2_10x3)
	Vectorize (f Word2_10x3) = f (V4 Word2_10x3)
	Vectorize (f a) = f a
	Vectorize a = V1 a
	-- Float -> (1, GL_GLOAT), Vec2 -> (2, GL_FLOAT)

type family VDim v :: Nat where
	VDim V1 = 1
	VDim V2 = 2
	VDim V3 = 3
	VDim V4 = 4

class VertexAttributeArray attr src where
	glVertexAttribPtr :: GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> Ptr (attr, src) -> GL ()

instance VertexAttributeArray Float a where
	glVertexAttribPtr i d t n s p = glVertexAttribPointer i d t n s (castPtr p)

-- | a = Int/Word32, b = Int/Word 8/16/32
instance (Integral a, Integral b) => VertexAttributeArray a b where
	glVertexAttribPtr i d t _ s p = glVertexAttribIPointer i d t s (castPtr p)

instance forall p a b v a' b'.
	( VertexAttribute a
	, Vectorize a ~ v a'
	, Vectorize b ~ v b'
	, KnownNat (VDim v)
	--, KnownNat (Aligned (SizeOf b))
	, AttrElement b'
	, VertexAttributeArray a' b' )
	=> AttrStruct (Attrib p a) p b where

	-- 'length' must be 1 (array of atrribute is not allowed.)
	-- 3.1 spec says 'normalize' is ignored for floating-point types
	-- such as GL_FIXED
	-- 'stride' == 0 means 'tightly packed'
	glVertexBuffer (Attrib (ix, length, normalize, divisor)) buf = do
		glEnableVertexAttribArray ix
		when (divisor /= 0) $ glVertexAttribDivisor ix divisor
		glVertexAttribPointer ix dim typ normalize stride nullPtr
		where dim = fromIntegral $ natVal (Proxy :: Proxy (VDim v))
		      typ = glType (Proxy :: Proxy b')
		      stride = 0 --fromIntegral $ natVal (Proxy :: Proxy (SizeOf b))

-- XXX (V4 Int2_10x3) (V3 Word2_10x3) cannot match here
instance
	( VertexAttribute (f (g a))
	, Applicative g
	, Floating a -- Float or Double
	, FoldableWithIndex (E V4) g
	, KnownNat (VDim f)
	, KnownNat (SizeOf (f a))
	, KnownNat (SizeOf (f (g a))) -- (Aligned (SizeOf (f (g a))))
	, AttrElement a )
	=> AttrStruct (Attrib p (f (g a))) p (f (g a)) where

	glVertexBuffer (Attrib (index, length, normalize, divisor)) buf = do
		iforM_ (pure () :: g ()) $ \(E e) _ -> do
			let i = (V4 0 1 2 3)^.e
			let ix = index + fromIntegral i
			glEnableVertexAttribArray ix
			when (divisor /= 0) $ glVertexAttribDivisor ix divisor
			glVertexAttribPointer ix dim typ normalize stride (plusPtr nullPtr (i * size))
		where dim = fromIntegral $ natVal (Proxy :: Proxy (VDim f))
		      typ = glType (Proxy :: Proxy a)
		      size = fromIntegral $ natVal (Proxy :: Proxy (SizeOf (f a)))
		      stride = fromIntegral $ natVal (Proxy :: Proxy (SizeOf (f (g a)))) -- (Aligned (SizeOf (f (g a))))


-- XXX 4byte alignment
-- | Transpose matrices
class Storable a => GLStorable a where
	pokeArrayGL :: Ptr a -> [a] -> GL ()
instance (Storable (f (g a)), Storable (g (f a)), VertexAttribute (f (g Float)), Functor f, Distributive g)
	=> GLStorable (f (g a)) where
	pokeArrayGL ptr xs = pokeArray (castPtr ptr) (map distribute xs)
instance Storable a => GLStorable a where
	pokeArrayGL = pokeArray

--class AttrStruct a where
--	pokeAll :: Ptr () -> a -> IO ()

--instance AttrVal a => AttrStruct a where
--	pokeAll p a = poke (castPtr p) a
--instance (AttrVal a, AttrVal b) => AttrStruct (a, b) where
--	pokeAll p (a,b) = do
--		poke (castPtr p) a
--		pokeByteOff (castPtr p) (alignment a) b
--instance (AttrVal a, AttrVal b, AttrVal c) => AttrStruct (a, b, c) where
--	pokeAll p (a,b,c) = do
--		poke (castPtr p) a
--		pokeByteOff (castPtr p) offset1 b
--		pokeByteOff (castPtr p) offset2 c
--		where offset1 = alignment a; offset2 = offset1 + alignment b
--instance (AttrVal a, AttrVal b, AttrVal c, AttrVal d) => AttrStruct (a, b, c, d)
--instance (AttrVal a, AttrVal b, AttrVal c, AttrVal d, AttrVal e) => AttrStruct (a, b, c, d, e)
--instance (AttrVal a, AttrVal b, AttrVal c, AttrVal d, AttrVal e, AttrVal f) => AttrStruct (a, b, c, d, e, f)
--instance (AttrVal a, AttrVal b, AttrVal c, AttrVal d, AttrVal e, AttrVal f, AttrVal g) => AttrStruct (a, b, c, d, e, f, g)
--instance (AttrVal a, AttrVal b, AttrVal c, AttrVal d, AttrVal e, AttrVal f, AttrVal g, AttrVal h) => AttrStruct (a, b, c, d, e, f, g, h)
--instance (AttrVal a, AttrVal b, AttrVal c, AttrVal d, AttrVal e, AttrVal f, AttrVal g, AttrVal h, AttrVal i) => AttrStruct (a, b, c, d, e, f, g, h, i)
--instance (AttrVal a, AttrVal b, AttrVal c, AttrVal d, AttrVal e, AttrVal f, AttrVal g, AttrVal h, AttrVal i, AttrVal j) => AttrStruct (a, b, c, d, e, f, g, h, i, j)

