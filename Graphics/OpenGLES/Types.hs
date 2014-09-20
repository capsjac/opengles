{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.OpenGLES.Types
  ( module Linear.Class
  , module Linear.Vect
  , module Linear.Mat
  , Vec2, Vec3, Vec4
  , BVec2, BVec3, BVec4
  , IVec2, IVec3, IVec4
  , UVec2, UVec3, UVec4
  , Mat2, Mat3, Mat4
  , Mat2x3, Mat2x4, Mat3x2
  , Mat3x4, Mat4x2, Mat4x3
  , HalfFloat(..), FixedFloat(..), Int10x3_2(..), Word10x3_2(..)
  , glType
  )
  where
import Linear.Class
import Linear.Vect
import Linear.Mat
import Foreign

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

--data M2 a = M2 a a a a
--	deriving (Show)
--data M3 a = M3 a a a a a a a a a
--	deriving (Show)
--data M4 a = M4 a a a a a a a a a a a a a a a a
--	deriving (Show)
--data M2x3 a = M2x3 a a a a a a
--	deriving (Show)
--data M2x4 a = M2x4 a a a a a a a a
--	deriving (Show)
--data M3x2 a = M3x2 a a a a a a
--	deriving (Show)
--data M3x4 a = M3x4 a a a a a a a a a a a a
--	deriving (Show)
--data M4x2 a = M4x2 a a a a a a a a
--	deriving (Show)
--data M4x3 a = M4x3 a a a a a a a a a a a a
--	deriving (Show)


-- * Vertex Attributes

newtype HalfFloat = HalfFloat Word16 deriving (Num,Read,Show,Storable)
newtype FixedFloat = FixedFloat Int32 deriving (Num,Read,Show,Storable)
newtype Int10x3_2 = Int10x3_2 Int32 deriving (Num,Read,Show,Storable)
newtype Word10x3_2 = Word10x3_2 Int32 deriving (Num,Read,Show,Storable)

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

{-
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
-}

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

{-
instance Storable a => Storable (T2 a) where
	sizeOf (T2 x _) = sizeOf x * 2
	alignment (T2 x _) = (sizeOf x * 2 + 3) .&. 12
	peek = undefined
	poke p' (T2 x y) = poke p x >> pokeElemOff p 1 y
		where p = castPtr p'

instance Storable a => Storable (T3 a) where
	sizeOf (T3 x _ _) = sizeOf x * 3
	alignment (T3 x _ _) = (sizeOf x * 3 + 3) .&. 12
	peek = undefined
	poke p' (T3 x y z) = do
		poke p x
		pokeElemOff p 1 y
		pokeElemOff p 2 z
		where p = castPtr p'

instance Storable a => Storable (T4 a) where
	sizeOf (T4 x _ _ _) = sizeOf x * 4
	alignment (T4 x _ _ _) = sizeOf x * 4
	peek = undefined
	poke p' (T4 x y z w) = do
		poke p x
		pokeElemOff p 1 y
		pokeElemOff p 2 z
		pokeElemOff p 3 w
		where p = castPtr p'

instance Storable a => Storable (M2 a) where
	sizeOf (M2 a _ _ _) = sizeOf a * 4
	alignment = sizeOf
	peek = undefined
	poke p (M2 a b c d) = pokeArray (castPtr p) [a,d, c,a]

instance Storable a => Storable (M3 a) where
	sizeOf (M3 a _ _ _ _ _ _ _ _) = sizeOf a * 9
	alignment = sizeOf
	peek = undefined
	poke p (M3 a b c d e f g h i) =
		pokeArray (castPtr p) [a,d,g, b,e,h, c,f,i]

instance Storable a => Storable (M4 a) where
	sizeOf (M4 a _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = sizeOf a * 16
	alignment = sizeOf
	peek = undefined
	poke t (M4 a b c d e f g h i j k l m n o p) =
		pokeArray (castPtr t) [a,e,i,m, b,f,j,n, c,g,k,o, d,h,l,p]

instance Storable a => Storable (M2x3 a) where
	sizeOf (M2x3 a _ _ _ _ _) = sizeOf a * 6
	alignment = sizeOf
	peek = undefined
	poke p (M2x3 a b c d e f) =
		pokeArray (castPtr p) [a,d, b,e, c,f]

instance Storable a => Storable (M2x4 a) where
	sizeOf (M2x4 a _ _ _ _ _ _ _) = sizeOf a * 8
	alignment = sizeOf
	peek = undefined
	poke p (M2x4 a b c d e f g h) =
		pokeArray (castPtr p) [a,e, b,f, c,g, d,h]

instance Storable a => Storable (M3x2 a) where
	sizeOf (M3x2 a _ _ _ _ _) = sizeOf a * 6
	alignment = sizeOf
	peek = undefined
	poke p (M3x2 a b c d e f) =
		pokeArray (castPtr p) [a,c,e, b,d,f]

instance Storable a => Storable (M3x4 a) where
	sizeOf (M3x4 a _ _ _ _ _ _ _ _ _ _ _) = sizeOf a * 12
	alignment = sizeOf
	peek = undefined
	poke p (M3x4 a b c d e f g h i j k l) =
		pokeArray (castPtr p) [a,e,i, b,f,j, c,g,k, d,h,l]

instance Storable a => Storable (M4x2 a) where
	sizeOf (M4x2 a _ _ _ _ _ _ _) = sizeOf a * 8
	alignment = sizeOf
	peek = undefined
	poke p (M4x2 a b c d e f g h) =
		pokeArray (castPtr p) [a,c,e,g, b,d,f,h]

instance Storable a => Storable (M4x3 a) where
	sizeOf (M4x3 a _ _ _ _ _ _ _ _ _ _ _) = sizeOf a * 12
	alignment = sizeOf
	peek = undefined
	poke p (M4x3 a b c d e f g h i j k l) =
		pokeArray (castPtr p) [a,d,g,j, b,e,h,k, c,f,i,l]
-}


class GLType a where
	glType :: m a -> Word32

instance GLType Int8 where glType _ = 0x1400
instance GLType Word8 where glType _ = 0x1401
instance GLType Int16 where glType _ = 0x1402
instance GLType Word16 where glType _ = 0x1403
instance GLType Int32 where glType _ = 0x1404
instance GLType Word32 where glType _ = 0x1405

instance GLType Float where glType _ = 0x1406
instance GLType HalfFloat where glType _ = 0x140B
instance GLType FixedFloat where glType _ = 0x140C
instance GLType Int10x3_2 where glType _ = 0x8D9F
instance GLType Word10x3_2 where glType _ = 0x8368

