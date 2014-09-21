{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Graphics.OpenGLES.Types (
  -- * Shading Language Base Types
  Vec2, Vec3, Vec4,
  BVec2, BVec3, BVec4,
  IVec2, IVec3, IVec4,
  UVec2, UVec3, UVec4,
  Mat2, Mat3, Mat4,
  Mat2x3, Mat2x4, Mat3x2,
  Mat3x4, Mat4x2, Mat4x3,

  -- * Uniform Variable
  Uniform, UnifVal,
  
  -- * Vertex Attribute
  Attrib, ShaderAttribute, AttrStruct,
  
  -- * Vertex Attribute Array Source Datatypes
  HalfFloat(..), FixedFloat(..),
  Int10x3_2(..), Word10x3_2(..)
  ) where
import Control.Monad (when)
import Foreign
import Linear.Class (Transpose, transpose)
import Linear.Vect
import Linear.Mat
import Graphics.OpenGLES.Base
import Graphics.OpenGLES.Internal


type Vec2 = V2 Float
type Vec3 = V3 Float
type Vec4 = V4 Float
type BVec2 = V2 Bool
type BVec3 = V3 Bool
type BVec4 = V4 Bool
type IVec2 = V2 Int32
type IVec3 = V3 Int32
type IVec4 = V4 Int32
type UVec2 = V2 Word32
type UVec3 = V3 Word32
type UVec4 = V4 Word32
type Mat2 = M2 Float
type Mat3 = M3 Float
type Mat4 = M4 Float
type Mat2x3 = M2x3 Float
type Mat2x4 = M2x4 Float
type Mat3x2 = M3x2 Float
type Mat3x4 = M3x4 Float
type Mat4x2 = M4x2 Float
type Mat4x3 = M4x3 Float


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

--instance UnifVal [Float] where
--	glUniform (loc, len, ptr) values = do
--		let len' = fromIntegral len
--		pokeArray (castPtr ptr :: Ptr Float) (take len' values)
--		glUniform1fv loc len (castPtr ptr)

pokeUniformArray
	:: Storable b => (GLint -> GLsizei -> Ptr a -> GL ())
	-> (GLint, GLsizei, Ptr ()) -> [b] -> GL ()
pokeUniformArray glUniformV (loc, len, ptr) values = do
	let len' = fromIntegral len
	pokeArray (castPtr ptr :: Ptr b) (take len' values)
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

-- 'transpose' argument must be GL_FALSE in GL ES 2.0
pokeMatrix :: (Transpose a b, Storable b)
	=> (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
	-> (GLint, GLsizei, Ptr ()) -> a -> GL ()
pokeMatrix glUniformMatrixV (loc, 1, ptr) matrix = do
	poke (castPtr ptr :: Ptr b) (transpose matrix)
	glUniformMatrixV loc 1 0 (castPtr ptr)
pokeMatrix _ _ _ = return () -- poke to nullPtr


instance UnifVal Mat2 where glUniform = pokeMatrix glUniformMatrix2fv
instance UnifVal Mat3 where glUniform = pokeMatrix glUniformMatrix3fv
instance UnifVal Mat4 where glUniform = pokeMatrix glUniformMatrix4fv

-- GL ES 3.0+ supports transpose
pokeMatrixT :: Storable a
	=> (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
	-> (GLint, GLsizei, Ptr ()) -> a -> GL ()
pokeMatrixT glUniformMatrixV (loc, 1, ptr) matrix = do
	poke (castPtr ptr :: Ptr a) matrix
	glUniformMatrixV loc 1 1 (castPtr ptr)
pokeMatrixT _ _ _ = return ()

-- http://delphigl.de/glcapsviewer/gles_extensions.php 
instance UnifVal Mat2x3 where glUniform = pokeMatrixT glUniformMatrix2x3fv
instance UnifVal Mat2x4 where glUniform = pokeMatrixT glUniformMatrix2x4fv
instance UnifVal Mat3x2 where glUniform = pokeMatrixT glUniformMatrix3x2fv
instance UnifVal Mat3x4 where glUniform = pokeMatrixT glUniformMatrix3x4fv
instance UnifVal Mat4x2 where glUniform = pokeMatrixT glUniformMatrix4x2fv
instance UnifVal Mat4x3 where glUniform = pokeMatrixT glUniformMatrix4x3fv

-- 'transpose' argument must be GL_FALSE in GL ES 2.0
pokeMatrices :: (Transpose a b, Storable b)
	=> (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
	-> (GLint, GLsizei, Ptr ()) -> [a] -> GL ()
pokeMatrices glUniformMatrixV (loc, len, ptr) matrices = do
	let len' = fromIntegral len
	pokeArray (castPtr ptr :: Ptr b)
		(map transpose $ take len' matrices) -- maybe slow
	glUniformMatrixV loc len 0 (castPtr ptr)

instance UnifVal [Mat2] where glUniform = pokeMatrices glUniformMatrix2fv
instance UnifVal [Mat3] where glUniform = pokeMatrices glUniformMatrix3fv
instance UnifVal [Mat4] where glUniform = pokeMatrices glUniformMatrix4fv

-- GL ES 3.0+ supports transpose
pokeMatricesT :: Storable a
	=> (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
	-> (GLint, GLsizei, Ptr ()) -> [a] -> GL ()
pokeMatricesT glUniformMatrixV (loc, len, ptr) matrices = do
	let len' = fromIntegral len
	pokeArray (castPtr ptr :: Ptr a) (take len' matrices)
	glUniformMatrixV loc len 1 (castPtr ptr)

instance UnifVal [Mat2x3] where glUniform = pokeMatricesT glUniformMatrix2x3fv
instance UnifVal [Mat2x4] where glUniform = pokeMatricesT glUniformMatrix2x4fv
instance UnifVal [Mat3x2] where glUniform = pokeMatricesT glUniformMatrix3x2fv
instance UnifVal [Mat3x4] where glUniform = pokeMatricesT glUniformMatrix3x4fv
instance UnifVal [Mat4x2] where glUniform = pokeMatricesT glUniformMatrix4x2fv
instance UnifVal [Mat4x3] where glUniform = pokeMatricesT glUniformMatrix4x3fv


-- Array of attributes is not supported in GLSL ES

instance GenericVertexAttribute a => ShaderAttribute a where
	glVertexAttrib idx x =
		with (V4 x 0 0 1) $ glVertexAttrib4v idx
instance GenericVertexAttribute a => ShaderAttribute (V2 a) where
	glVertexAttrib idx (V2 x y) =
		with (V4 x y 0 1) $ glVertexAttrib4v idx
instance GenericVertexAttribute a => ShaderAttribute (V3 a) where
	glVertexAttrib idx (V3 x y z) =
		with (V4 x y z 1) $ glVertexAttrib4v idx
instance GenericVertexAttribute a => ShaderAttribute (V4 a) where
	glVertexAttrib idx v4 =
		with v4 $ glVertexAttrib4v idx
instance ShaderAttribute Mat2 where
	glVertexAttrib idx (M2 (V2 a b) (V2 c d)) = do
		with (V4 a c 0 1) $ glVertexAttrib4v idx
		with (V4 b d 0 1) $ glVertexAttrib4v (idx + 1)
instance ShaderAttribute Mat3 where
	glVertexAttrib idx (M3 (V3 a b c) (V3 d e f) (V3 g h i)) = do
		with (V4 a d g 1) $ glVertexAttrib4v idx
		with (V4 b e h 1) $ glVertexAttrib4v (idx + 1)
		with (V4 c f i 1)  $ glVertexAttrib4v (idx + 2)
instance ShaderAttribute Mat4 where
	glVertexAttrib idx (M4 (V4 a b c d) (V4 e f g h) (V4 i j k l) (V4 m n o p)) = do
		with (V4 a e i m) $ glVertexAttrib4v idx
		with (V4 b f j n) $ glVertexAttrib4v (idx + 1)
		with (V4 c g k o) $ glVertexAttrib4v (idx + 2)
		with (V4 d h l p) $ glVertexAttrib4v (idx + 3)
-- XXX I'm not sure below types are actually supported by the GL
instance ShaderAttribute Mat3x2 where
	glVertexAttrib idx (M3x2 a b  c d  e f) = do
		with (V4 a c e 1) $ glVertexAttrib4v idx
		with (V4 b d f 1) $ glVertexAttrib4v (idx + 1)
instance ShaderAttribute Mat4x2 where
	glVertexAttrib idx (M4x2 a b  c d  e f  g h) = do
		with (V4 a c e g) $ glVertexAttrib4v idx
		with (V4 b d f h) $ glVertexAttrib4v (idx + 1)
instance ShaderAttribute Mat2x3 where
	glVertexAttrib idx (M2x3 a b c  d e f) = do
		with (V4 a d 0 1) $ glVertexAttrib4v idx
		with (V4 b e 0 1) $ glVertexAttrib4v (idx + 1)
		with (V4 c f 0 1) $ glVertexAttrib4v (idx + 2)
instance ShaderAttribute Mat4x3 where
	glVertexAttrib idx (M4x3 a b c  d e f  g h i  j k l) = do
		with (V4 a d g j) $ glVertexAttrib4v idx
		with (V4 b e h k) $ glVertexAttrib4v (idx + 1)
		with (V4 c f i l) $ glVertexAttrib4v (idx + 2)
instance ShaderAttribute Mat2x4 where
	glVertexAttrib idx (M2x4 a b c d  e f g h) = do
		with (V4 a e 0 1) $ glVertexAttrib4v idx
		with (V4 b f 0 1) $ glVertexAttrib4v (idx + 1)
		with (V4 c g 0 1) $ glVertexAttrib4v (idx + 2)
		with (V4 d h 0 1) $ glVertexAttrib4v (idx + 3)
instance ShaderAttribute Mat3x4 where
	glVertexAttrib idx (M3x4 a b c d  e f g h  i j k l) = do
		with (V4 a e i 1) $ glVertexAttrib4v idx
		with (V4 b f j 1) $ glVertexAttrib4v (idx + 1)
		with (V4 c g k 1) $ glVertexAttrib4v (idx + 2)
		with (V4 d h l 1) $ glVertexAttrib4v (idx + 3)


--(GLuint indx, GLint size, GLenum type, GLboolean normalized, GLsizei stride, const GLvoid* ptr)
instance AttrStruct Float (Attrib p Float) p where
	glVertexAttribPtr (Attrib (idx, 1{-size-}, normalized, divisor)) buf = do
		glEnableVertexAttribArray idx
		when (divisor /= 0) $
			glVertexAttribDivisor idx divisor
		-- XXX 3.1 spec says normalize is ignored for floating-point types, really?
		glVertexAttribPointer idx 1 (glType buf) normalized 0 nullPtr
	glVertexAttribPtr attr buf = glLog $ "Ignoring attirb: " ++ show attr
instance AttrStruct Vec2 (Attrib p Vec2) p where
	glVertexAttribPtr (Attrib (idx, 1{-size-}, normalized, divisor)) buf = do
		glEnableVertexAttribArray idx
		when (divisor /= 0) $ glVertexAttribDivisor idx divisor
		glVertexAttribPointer idx 2 (glType ([] :: [Float])) normalized 0 nullPtr
	glVertexAttribPtr attr buf = glLog $ "Ignoring attirb: " ++ show attr
instance AttrStruct (V2 Word8) (Attrib p Vec2) p where
	glVertexAttribPtr (Attrib (idx, 1{-size-}, normalized, divisor)) buf = do
		glEnableVertexAttribArray idx
		when (divisor /= 0) $ glVertexAttribDivisor idx divisor
		glVertexAttribPointer idx 2 (glType ([] :: [Word8])) normalized 0 nullPtr
	glVertexAttribPtr attr buf = glLog $ "Ignoring attirb: " ++ show attr

{-
class (Storable a, Num a) => AttrElement a where
instance AttrElement Word8
instance AttrElement Word16
instance AttrElement Word32
instance AttrElement Int8
instance AttrElement Int16
instance AttrElement Int32
instance AttrElement Float
instance AttrElement HalfFloat
instance AttrElement FixedFloat

case splitTyConApp values of
getrepl (con, args)
	| con == float = [(1, 4, flo)]
	| con == t2 = [(2, 2*size, typ)] where [(_,size,typ)] = getrepl $ head args
	| con == m2 = [(2, 2*size, typ), (2, 2*size, typ)] where [(_,size,typ)] = getrepl . head $ args
	| con == Int10x3_2 = [(4,4,Int10x3_2)]
	| otherwise = concatMap getrepl args
Float
-> [(1, 4, flo)]
Vec2
-> [(2, 8, flo)]
Mat2
-> [(2, 8, flo), (2, 8, flo)]
((,) Word8 Word8)
-> [(1, 4, word8), (1, 4, word8)]
((,) (M2 Float) (M2 Word8))
-> [(2, 8, flo), (2, 8, flo), (2, 2, word8), (2, 2, word8)]
Int10x3_2
-> [(4, 4, Int10x3_2)]
stride = sum $ map snd3 types
foldl (\offset (size, align, typ)-> glVertexAttribPointer ...>> return offset+align) 0 types
data = Spl | Mat
word8 = typeRep (Proxy :: Proxy Word8)
word16 = typeRep (Proxy :: Proxy Word16)
word32 = typeRep (Proxy :: Proxy Word32)
int8 = typeRep (Proxy :: Proxy Int8)
int16 = typeRep (Proxy :: Proxy Int16)
int32 = typeRep (Proxy :: Proxy Int32)
float = typeRep (Proxy :: Proxy Float)

class Storable a => AttrVal a where
-- scalar
instance AttrElement a => AttrVal a
-- vector
instance AttrElement a => AttrVal (V2 a)
instance AttrElement a => AttrVal (V3 a)
instance AttrElement a => AttrVal (V4 a)
instance AttrVal Int10x3_2
instance AttrVal Word10x3_2
-- matrix
instance AttrElement a => AttrVal (M2 a)
instance AttrElement a => AttrVal (M3 a)
instance AttrElement a => AttrVal (M4 a)
instance AttrElement a => AttrVal (M2x3 a)
instance AttrElement a => AttrVal (M2x4 a)
instance AttrElement a => AttrVal (M3x2 a)
instance AttrElement a => AttrVal (M3x4 a)
instance AttrElement a => AttrVal (M4x2 a)
instance AttrElement a => AttrVal (M4x3 a)
{-- matrix (colomn major)
instance AttrVal (T2 Int10x3_2)
instance AttrVal (T3 Int10x3_2)
instance AttrVal (T4 Int10x3_2)
instance AttrVal (T2 Word10x3_2)
instance AttrVal (T3 Word10x3_2)
instance AttrVal (T4 Word10x3_2)
-}

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

--instance Storable FixedFloat where
--	sizeOf _ = 4; alignment _ = 4; peek = undefined
--	poke p (FixedFloat x) = poke (castPtr p) x

--instance Storable Int10x3_2 where
--	sizeOf _ = 4; alignment _ = 4; peek = undefined
--	poke p (Int10x3_2 x) = poke (castPtr p) x

--instance Storable Word10x3_2 where
--	sizeOf _ = 4; alignment _ = 4; peek = undefined
--	poke p (Word10x3_2 x) = poke (castPtr p) x

--instance Storable HalfFloat where
--	sizeOf _ = 2; alignment _ = 4; peek = undefined
--	poke p (HalfFloat x) = poke (castPtr p) x

-}


