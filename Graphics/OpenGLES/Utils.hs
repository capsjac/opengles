-- XXX better module name
module Graphics.OpenGLES.Utils where
import qualified Data.ByteString as B
import Graphics.OpenGLES
import Graphics.OpenGLES.Types

{-

Scene
Node
Mesh, Light, Camera, Buffer
Material
Technique
Shader

data GLScene = GLScene [Node]
data GLNode = GLNode [Mesh] [Light] Camera [Buffer]
data Mesh = Mesh
data Light = Light
data Camera = Camera
data Buffer = Buffer
data Material = Material
data Technique = Technique
data Program = Program (Maybe Int) VertexShader FragmentShader
data Shader = VertexShader | FragmentShader
 
data ArrayBuffer = ArrayBuffer Int
data VertexAttribute = VertexAttribute ArrayBuffer? gltype nbElm
data Primitive = Primitive idxOfs idxCount idxGlType primGlType VertexAttribIdx
data Mesh = Mesh [VertexAttribute] [Primitive]
data Program = Program glObj
data Shader = Shader
data Texture = Texture glObj image
data Tex2d = Tex2d
data Material = Material Parameters Technique
data Technique = Technique [Uniform] Program

-- resource descripter
data in = in BufferType binary offset length
data out = out BufferId itemsize numitems
prepareBuffer :: in -> IO out

data in = in Image
data out = out TextureId
prepareTexture :: in -> IO out

data in = in [VertexShader] [FragmentShader] 
data out = out ProgramId [ShaderId]
prepareProgram :: in -> IO out

^^^ in = Setup/Init Buffer/Tx/Prog
out = +Obj
-}

type Mesh = [DrawCall]

data DrawCall = DrawCall
	DrawMode
	Program
	Indices
	[VertexAttr]
	[UniformVar]
	[Texture]
	DrawConfig
	deriving (Show, Eq)
-- XXX: DrawTexture Extension

data Blob = Blob B.ByteString
	deriving (Show, Eq)

-- ** Vertex Indices

type Indices = Int

-- ** Vertex Attribute
--VertexAttr "a_position" F3 FloatT False 0 0 blob
data VertexAttr = VertexAttr
		{ attrVarName :: String
		, attrType :: AttrType
		, attrSrcType :: AttrSrcType
		, attrNormalize :: Bool
		, attrStride :: Int
		, attrOffset :: Int
		, attrArray :: Blob
		}
	| ConstantVA
		{ attrVarName :: String
		, attrValue :: AttrValue
		}
	deriving (Show, Eq)

data AttrType =
	F1 | F2 | F3 | F4 | I1 | I2 | I3 | I4 | UI1 | UI2 | UI3 | UI4
	deriving (Show, Eq)

data AttrSrcType =
	  ByteT
	| UByteT
	| ShortT
	| UShortT
	| FloatT
	| FixedT
	| IntT  -- ^ ES 3.0
	| UIntT -- ^ ES 3.0
	-- more
	deriving (Show, Eq)

data AttrValue =
	  Attr1f GLfloat
	| Attr2f GLfloat GLfloat
	| Attr3f GLfloat GLfloat GLfloat
	| Attr4f GLfloat GLfloat GLfloat GLfloat
	| Attr4i GLint GLint GLint GLint -- ^ ES 3.0
	| Attr4ui GLuint GLuint GLuint GLuint -- ^ ES 3.0
	deriving (Show, Eq)


-- ** Uniform Variable

data UniformVar = UniformVar
	{ uniVarName :: String
	, uniformValue :: UniformValue
	}
	deriving (Show, Eq)

data UniformValue =
	  Uniform1f GLfloat
	| Uniform2f GLfloat GLfloat
	| Uniform3f GLfloat GLfloat GLfloat
	| Uniform4f GLfloat GLfloat GLfloat GLfloat
	| Uniform1i GLint
	| Uniform2i GLint GLint
	| Uniform3i GLint GLint GLint
	| Uniform4i GLint GLint GLint GLint
	| UniformMatrix2 GLfloat GLfloat GLfloat GLfloat
	| UniformMatrix3 GLfloat GLfloat GLfloat GLfloat GLfloat GLfloat GLfloat GLfloat GLfloat
	| UniformMatrix4
	-- ES 3.0
	| Uniform1ui GLuint
	| Uniform2ui GLuint GLuint
	| Uniform3ui GLuint GLuint GLuint
	| Uniform4ui GLuint GLuint GLuint GLuint
	| UniformMatrix2x3
	| UniformMatrix3x2
	| UniformMatrix2x4
	| UniformMatrix4x2
	| UniformMatrix3x4
	| UniformMatrix4x3
	deriving (Show, Eq)


-- ** Shader

data Program = Program [Shader]
	deriving (Show, Eq)

data Shader = Shader
	{ s_type :: ShaderType
	, s_source :: Blob
	, s_attribs :: [String]
	, s_uniforms :: [String]
	}
	deriving (Show, Eq)


-- ** Texture

data Texture =
	Texture2D
		{ tex2dTarget :: Tex2DTarget
		, texFormat :: TextureFormat
		, texBitLayout :: TextureBitLayout
		, texInternalFormat :: TextureInternalFormat
		, texSampler :: Sampler
		, texWidth :: Int
		, texHeight :: Int
		, texBorder :: Int
		, texLevel :: Int
		}
	| Texture3D
		{ tex3dTarget :: Tex3DTarget
		, texFormat :: TextureFormat
		, texBitLayout :: TextureBitLayout
		, texInternalFormat :: TextureInternalFormat
		, texSampler :: Sampler
		, texWidth :: Int
		, texHeight :: Int
		, texBorder :: Int
		, texLevel :: Int
		}
	deriving (Show, Eq)

data Tex2DTarget = Tex2D
	| CubeMapPositiveX
	| CubeMapPositiveY
	| CubeMapPositiveZ
	| CubeMapNegativeX
	| CubeMapNegativeY
	| CubeMapNegativeZ
	deriving (Show, Eq)

-- 3.0
data Tex3DTarget = Tex3D | Tex2DArray
	deriving (Show, Eq)

data TextureFormat = ALPHA | RGB | RGBA | LUMINANCE | LUMINANCE_ALPHA
	deriving (Show, Eq)

data TextureBitLayout = UByte | US565 | US444 | US5551
	-- 3.0
	| Byte | UShort | Short | UInt | Int | HalfFloat | Float | US4444 | UI2_10_10_10Rev | UI24_8 | UI_10f11f11fRev | UI5999Rev | F32UI24_8Rev
	deriving (Show, Eq)

data TextureInternalFormat = Alpha | Rgb | Rgba | Luminance | LuminanceAlpha
	-- 3.0
	| R8 | R8i | R8ui | R8snorm | R16i | R16ui | R16f | R32i | R32ui | R32f
	| Rg8 | Rg8i | Rg8ui | Rg8snorm | Rg16i | Rg16ui | Rg16f | Rg32i | Rg32ui | Rg32f
	| Rgb8 | Rgb8i | Rgb8ui | Rgb8snorm | Rgb16i | Rgb16ui | Rgb16f | Rgb32i | Rgb32ui | Rgb32f
	| Rgba8 | Rgba8i | Rgba8ui | Rgba8snorm | Rgba16i | Rgba16ui | Rgba16f | Rgba32i | Rgba32ui | Rgba32f
	| Rgb5a1 | Rgb565 | Rgb9e5 | Rgb10a2 | Rgb10a2ui | Srgb8 | Rgba4 | Srgb8Alpha8
	| R11fG11fB10f | DepthComponent16 | DepthComponent24 | DepthComponent32
	| Depth24Stencil8 | Depth32fStencil8
	deriving (Show, Eq)


-- ** Sampler

data Sampler = SamplerES2
		{ magFilter :: MagFilter
		, minFilter :: MinFilter
		, wrapS :: WrapMode
		, wrapT :: WrapMode
		}
	-- | SamplerES3
	deriving (Show, Eq)

data MagFilter = MagNearest | MagLinear
	deriving (Show, Eq)

data MinFilter = MinNearest | MinLinear
	| NearestMipmapNearest | NearestMipmapLinear
	| LinearMipmapNearest | LinearMipmapLinear
	deriving (Show, Eq)

data WrapMode = Repeat | ClampToEdge | MirroredRepeat
	deriving (Show, Eq)


-- ** Graphic State

data DrawConfig = DrawConfig
	{ blendEnable :: Bool
	, cullFaceEnable :: Bool
	, depthMask :: Int
	, depthTextEnable :: Bool
	}
	deriving (Show, Eq)

-- call glPixelStorei GL_[UN]PACK_ALIGNMENT [1248] before texImage2d
