module Graphics.OpenGLES.Types where

import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Ptr

-- * OpenGL ES 2.0

-- | 1bit boolean
type GLboolean = CUChar

-- | 8bit signed two\'s complement binary integer
type GLbyte = CSChar

-- | 8bit unsigned binary integer
type GLubyte = CUChar

-- | 8bit characters making up strings
type GLchar = CChar

-- | 16bit signed two\'s complement binary integer
type GLshort = CShort

-- | 16bit unsigned binary integer
type GLushort = CUShort

-- | 32bit signed two\'s complement binary integer
type GLint = CInt

-- | 32bit unsigned binary integer
type GLuint = CUInt

-- | 32bit signed two\'s complement 16.16 scaled integer
type GLfixed = CInt

-- | 32bit non-negative binary integer size
type GLsizei = CInt

-- | 32bit enumerated binary integer value
type GLenum = CUInt

-- | Pointer-sized signed two\'s complement binary integer
type GLintptr = CPtrdiff

-- | Pointer-sized non-negative binary integer size
type GLsizeiptr = CPtrdiff

-- | 32bit bit field
type GLbitfield = CUInt

-- | 32bit floating-point value
type GLfloat = CFloat

-- | 32bit floating-point value clamped to [0,1]
type GLclampf = CFloat

-- * OpenGL ES 3.0

-- | 64bit signed two\'s complement binary integer
type GLint64 = Int64

-- | 64bit unsigned binary integer
type GLuint64 = Word64

-- | Pointer-sized sync object handle
type GLsync = Ptr ()

-- | 16bit half-precision floating-point value encoded in an unsigned scalar
type GLhalf = CUShort
