{-# LANGUAGE CPP, FlexibleInstances #-}
module Graphics.OpenGL.Types (
  
  -- * Shading Language Double Types
  DVec2, DVec3, DVec4,
  DMat2, DMat3, DMat4,
  DMat2x3, DMat2x4, DMat3x2,
  DMat3x4, DMat4x2, DMat4x3
  
  -- * Additional Instances

  ) where
import Foreign
import Graphics.OpenGL.CoreArb
import Graphics.OpenGLES.Base
import Graphics.OpenGLES.Internal
import Graphics.OpenGLES.Types
import Linear

type DVec2 = V2 Double
type DVec3 = V3 Double
type DVec4 = V4 Double
type DMat2 = M22 Double
type DMat3 = M33 Double
type DMat4 = M44 Double
type DMat2x3 = M23 Double
type DMat2x4 = M24 Double
type DMat3x2 = M32 Double
type DMat3x4 = M34 Double
type DMat4x2 = M42 Double
type DMat4x3 = M43 Double

#define Uniform(_typ, _arg, _suffix, _rhs) \
instance UnifVal (_typ) where \
	glUniform (loc, _, _) _arg = glUniform/**/_suffix loc _rhs \

Uniform(Double,x,1d,x)
Uniform(DVec2,(V2 x y),2d,x y)
Uniform(DVec3,(V3 x y z),3d,x y z)
Uniform(DVec4,(V4 x y z w),4d,x y z w)

{-# NOINLINE pokeUniformArray #-}
pokeUniformArray
	:: Storable b => (GLint -> GLsizei -> Ptr a -> GL ())
	-> (GLint, GLsizei, Ptr ()) -> [b] -> GL ()
pokeUniformArray glUniformV (loc, len, ptr) values = do
	pokeArray (castPtr ptr) (take (fromIntegral len) values)
	glUniformV loc len (castPtr ptr)

instance UnifVal [Double] where glUniform = pokeUniformArray glUniform1dv
instance UnifVal [DVec2] where glUniform = pokeUniformArray glUniform2dv
instance UnifVal [DVec3] where glUniform = pokeUniformArray glUniform3dv
instance UnifVal [DVec4] where glUniform = pokeUniformArray glUniform4dv

castMat a b c d e = a b c d (castPtr e)

instance UnifMat DMat2 where glUnifMat = castMat glUniformMatrix2dv
instance UnifMat DMat3 where glUnifMat = castMat glUniformMatrix3dv
instance UnifMat DMat4 where glUnifMat = castMat glUniformMatrix4dv
instance UnifMat DMat2x3 where glUnifMat = castMat glUniformMatrix2x3dv
instance UnifMat DMat2x4 where glUnifMat = castMat glUniformMatrix2x4dv
instance UnifMat DMat3x2 where glUnifMat = castMat glUniformMatrix3x2dv
instance UnifMat DMat3x4 where glUnifMat = castMat glUniformMatrix3x4dv
instance UnifMat DMat4x2 where glUnifMat = castMat glUniformMatrix4x2dv
instance UnifMat DMat4x3 where glUnifMat = castMat glUniformMatrix4x3dv

instance VertexAttribute Double where
	glVertexAttrib ix x = glVertexAttrib1d ix x
instance VertexAttribute DVec2 where
	glVertexAttrib ix (V2 x y) = glVertexAttrib2d ix x y
instance VertexAttribute DVec3 where
	glVertexAttrib ix (V3 x y z) = glVertexAttrib3d ix x y z
instance VertexAttribute DVec4 where
	glVertexAttrib ix (V4 x y z w) = glVertexAttrib4d ix x y z w

instance AttrElement Double

