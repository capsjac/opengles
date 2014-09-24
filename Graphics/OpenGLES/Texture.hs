{-# LANGUAGE RecordWildCards #-}
module Graphics.OpenGLES.Texture (
  -- * Texture
  Texture,
  TextureColorFormat,
  TextureBitLayout,
  TextureInternalFormat,
  TextureData,

  -- ** Sampler
  Sampler,
  MagFilter,
  MinFilter,
  WrapMode,

  glLoadKtxFile
  ) where
import Control.Applicative
import Control.Monad
import qualified Data.ByteString as B
import Data.Int
import Data.IORef
import Graphics.OpenGLES.Base
import Graphics.OpenGLES.Env
import Graphics.OpenGLES.Internal
import Graphics.TextureContainer.KTX
import Foreign.Ptr (castPtr)

data Texture = Texture GLO (IORef Ktx)
-- XXX Texture DoubleBufferring

data TextureColorFormat = ALPHA | RGB | RGBA | LUMINANCE | LUMINANCE_ALPHA

data TextureBitLayout = UByte | US565 | US444 | US5551
	-- | ES 3.0
	| Byte | UShort | Short | UInt | Int | HalfFloat | Float | US4444 | UI2_10_10_10Rev | UI24_8 | UI_10f11f11fRev | UI5999Rev | F32UI24_8Rev

data TextureInternalFormat = Alpha | Rgb | Rgba | Luminance | LuminanceAlpha
	-- 3.0
	| R8 | R8i | R8ui | R8snorm | R16i | R16ui | R16f | R32i | R32ui | R32f
	| Rg8 | Rg8i | Rg8ui | Rg8snorm | Rg16i | Rg16ui | Rg16f | Rg32i | Rg32ui | Rg32f
	| Rgb8 | Rgb8i | Rgb8ui | Rgb8snorm | Rgb16i | Rgb16ui | Rgb16f | Rgb32i | Rgb32ui | Rgb32f
	| Rgba8 | Rgba8i | Rgba8ui | Rgba8snorm | Rgba16i | Rgba16ui | Rgba16f | Rgba32i | Rgba32ui | Rgba32f
	| Rgb5a1 | Rgb565 | Rgb9e5 | Rgb10a2 | Rgb10a2ui | Srgb8 | Rgba4 | Srgb8Alpha8
	| R11fG11fB10f | DepthComponent16 | DepthComponent24 | DepthComponent32
	| Depth24Stencil8 | Depth32fStencil8

data TextureData =
	  PlainTexture
	| PVRTC -- PowerVR SGX(iOS), Samsung S5PC110(Galaxy S/Tab)
	| ATITC -- ATI Imageon, Qualcomm Adreno
	| S3TC -- NVIDIA Tegra2, ZiiLabs ZMS-08 HD
	| ETC -- Android, ARM Mali
	-- .| _3Dc -- ATI, NVidia
	-- .| Palette

-- | Load a GL Texture object from Ktx texture container.
-- Ref <https://github.com/KhronosGroup/KTX/blob/master/lib/loader.c>
loadKtx :: Maybe Texture -> Ktx -> GL Texture
loadKtx Nothing ktx@Ktx{..} = do
	--putStrLn.show$ktx
	let newTexture target = Texture
		<$> newGLO glGenTextures (glBindTexture target) glDeleteTextures
		<*> newIORef ktx
	case checkKtx ktx of
		Left msg -> glLog (msg ++ ": " ++ ktxName) >> newTexture 0
		Right (comp, target, genmip) -> do
			putStrLn . show $ (comp, target,genmip)
			tex <- newTexture target
			let fmt = if comp || sizedFormats
				then ktxGlInternalFormat
				else ktxGlBaseInternalFormat
			let uploadMipmap _ _ _ _ [] = return ()
			    uploadMipmap level w h d (faces:rest) = do
				case (comp, target) of
					-- 2D Compressed
					(True, 0x0DE1) -> do
						B.useAsCStringLen (head faces) $ \(ptr, len) ->
							glCompressedTexImage2D target level fmt w h 0 (i len) (castPtr ptr)
					-- 2D Uncompressed
					(False, 0x0DE1) -> do
						B.useAsCString (head faces) $ \ptr ->
							glTexImage2D target level fmt w h 0 ktxGlFormat ktxGlType (castPtr ptr)
					-- Cube Map Compressed
					(True, 0x8513) -> do
						forM_ (zip [texture_cube_map_positive_x..] faces) $ \(face, image) ->
							B.useAsCStringLen image $ \(ptr, len) ->
								glCompressedTexImage2D face level fmt w h 0 (i len) (castPtr ptr)
					-- Cube Map Uncompressed
					(False, 0x8513) -> do
						forM_ (zip [texture_cube_map_positive_x..] faces) $ \(face, image) ->
							B.useAsCString image $ \ptr ->
								glTexImage2D face level fmt w h 0 ktxGlFormat ktxGlType (castPtr ptr)
					-- 3D Compressed
					(True, 0x806F) -> do
						B.useAsCStringLen (head faces) $ \(ptr, len) ->
							glCompressedTexImage3D target level fmt w h d 0 (i len) (castPtr ptr)
					-- 3D Uncompressed
					(False, 0x806F) -> do
						B.useAsCString (head faces) $ \ptr ->
							glTexImage3D target level fmt w h d 0 ktxGlFormat ktxGlType (castPtr ptr)
					-- 2D Array Compressed
					(True, 0x8C1A) -> do
						B.useAsCStringLen (head faces) $ \(ptr, len) ->
							glCompressedTexImage3D target level fmt w h ktxNumElems 0 (i len) (castPtr ptr)
					-- 2D Array Uncompressed
					(False, 0x8C1A) -> do
						B.useAsCString (head faces) $ \ptr ->
							glTexImage3D target level fmt w h ktxNumFaces 0 ktxGlFormat ktxGlType (castPtr ptr)
				showError $ "gl{,Compressed}TexImage{2D,3D} " ++ ktxName
				uploadMipmap (level + 1) (div2 w) (div2 h) (div2 d) rest
				
				where
					div2 x = max 1 (div x 2)
					i = fromIntegral

			uploadMipmap 0 ktxPixelWidth ktxPixelHeight ktxPixelDepth ktxImage
			when genmip $ do
				glGenerateMipmap target
				void $ showError $ "glGenerateMipmap " ++ ktxName
			return tex

glLoadKtxFile :: FilePath -> GL Texture
glLoadKtxFile path = ktxFromFile path >>= loadKtx Nothing

--loadKtx tex@(Just (Texture glo ref)) ktx = do
--	checkKtx ktx ? iffail
--	writeIORef ktx
--	return tex
-- /* reject 3D texture if unsupported */
-- glPixelStorei(GL_UNPACK_ALIGNMENT, 4)
--hasES2 =
--	supportsSwizzle = GL_FALSE;
--	sizedFormats = _NO_SIZED_FORMATS;
--	R16Formats = _KTX_NO_R16_FORMATS;
--	supportsSRGB = GL_FALSE;
--hasES3 = sizedFormats = _NON_LEGACY_FORMATS;
--hasExt "GL_OES_required_internalformat" =
--	sizedFormats |= _ALL_SIZED_FORMATS
sizedFormats = hasES3 || hasExt "GL_OES_required_internalformat"

-- There are no OES extensions for sRGB textures or R16 formats.


-- | Check a KTX file header.
-- returning (isCompressed?, textureTarget, generateMipmapNeeded?)
checkKtx :: Ktx -> Either String (Bool, GLenum, Bool)
checkKtx t = (,,) <$> isCompressed t <*> detectTarget t <*> needsGenMipmap t

isCompressed Ktx { ktxGlTypeSize = s } | s /= 1 && s /= 2 && s /= 4 = Left "checkKtx: Invalid glTypeSize"
isCompressed Ktx { ktxGlType = 0, ktxGlFormat = 0 } = Right True
isCompressed Ktx { ktxGlType = t, ktxGlFormat = f } | t == 0 || f == 0 = Left "checkKtx: Invalid glType"
isCompressed _ = Right False

detectTarget ktx@Ktx { ktxNumElems = 0 } = detectCube ktx
detectTarget ktx = detectCube ktx >>= \target ->
	if target == texture_2d
		then Right texture_2d_array
		else Left "checkKtx: No API for 3D and cube arrays yet"
detectCube ktx@Ktx { ktxNumFaces = 1 } = detectDim ktx
detectCube ktx@Ktx { ktxNumFaces = 6 } = detectDim ktx >>= \dim ->
	if dim == 2
		then Right texture_cube_map
		else Left "checkKtx: cube map needs 2D faces"
detectCube _ = Left "checkKtx: numberOfFaces must be either 1 or 6"

detectDim Ktx { ktxPixelWidth = 0 } = Left "checkKtx: texture must have width"
detectDim Ktx { ktxPixelHeight = 0, ktxPixelDepth = 0 } = Left "checkKtx: 1D texture is not supported"
detectDim Ktx { ktxPixelHeight = 0 } = Left "checkKtx: texture must have height if it has depth"
detectDim Ktx { ktxPixelDepth = 0 } = Right texture_2d
detectDim _ = Right texture_3d

needsGenMipmap Ktx { ktxNumMipLevels = 0 } = Right True
needsGenMipmap Ktx {..}
	| max (max ktxPixelDepth ktxPixelHeight) ktxPixelDepth < 2^(ktxNumMipLevels-1)
	= Left "checkKtx: Can't have more mip levels than 1 + log2(max(width, height, depth))"
needsGenMipmap _ = Right False

texture_2d = 0x0DE1
texture_cube_map = 0x8513
texture_2d_array = 0x8C1A
texture_3d = 0x806F

texture_cube_map_positive_x = 0x8515


-- ** Sampler

-- 2d vs. 3d / mag + min vs. max
-- | (Texture wrap mode, A number of ANISOTROPY filter sampling points
-- (specify 0 to disable anisotropy filter), (Fallback) Mag and Min filters).
-- 
-- When /EXT_texture_filter_anisotropic/ is not supported, fallback filters
-- are used instead.
data Sampler =
	  Sampler2D (WrapMode, WrapMode) Int32 (MagFilter, MinFilter)
	| Sampler3D (WrapMode, WrapMode, WrapMode) Int32 (MagFilter, MinFilter)

newtype MagFilter = MagFilter Int32
magNearest = MagFilter 0x2600
magLinear = MagFilter 0x2601

-- TODO NoMipmap => use minLinear instead
newtype MinFilter = MinFilter Int32
minNearest = MinFilter 0x2600
minLinear = MinFilter 0x2601
nearestMipmapNearest = MinFilter 0x2700
linearMipmapNearest = MinFilter 0x2701
nearestMipmapLinear = MinFilter 0x2702
linearMipmapLinear = MinFilter 0x2703

-- TODO: NPOT => ClampToEdge only check / has ext?
newtype WrapMode = WrapMode Int32
repeatTexture = WrapMode 0x2901
clampToEdge = WrapMode 0x812F
mirroredRepeat = WrapMode 0x8370


-- mipmaps, compressed, fallbacks
-- [comptex, fallback]
-- glm basepath = /data/app.name/assets/...
-- glm preffered compression type (filename suffiex)
---- iOS: PVRTC
---- Android with Alpha: PVRTC+ATITC+S3TC+Uncompressed
---- Android without Alpha: ETC1
---- PC: S3TC + Uncompressed
--glm detect compression support
--glm detect max filter support
-- record Gen/Del/Draw* calls and responces
-- prefferedformat == "etc1" like.
-- texture from file "name" [path..] genMipmap?flag
-- feedback record, framebuffer management with glViewport?
-- Viewport....,BindFramebuffer,BindEGLContext,UnbindEGL
--Utils.hs shrinked triangles, 
--  Update[Sub]Texture,UpdateVertex(Buffer)
-- "texname" [tex1,tex2,...] auto select
-- auto alignment of texture data
-- texture atlas managment
{-
data Texture =
	  RawTexture String GLenum GLint GLint GLsizei GLsizei GLenum GLenum
  	-- ^ name, target, level, internalFormat, width, height, format, type
	| TextureFile
		{ texName :: String
		, texSampler :: Sampler
		, texUnit :: Int
		, texBlob :: B.ByteString
		, texGenMipmap :: Bool
		}
	| TextureFile'
		{ texName :: String
		, texFile :: B.ByteString
		, texName2 :: String
		, texFile2 :: B.ByteString
		, texSampler :: Sampler
		, texUnit :: Int
		, texGenMipmap :: Bool
		}
	| Texture
		{ texName :: String
		, texTarget :: TextureTarget
		, texLevel :: Int
		, texInternalFormat :: TextureInternalFormat
		, texWidth :: Int
		, texHeight :: Int
		, texBitLayout :: TextureBitLayout
		, texSampler :: Sampler
		, texUnit :: Int -- Maybe Int 0..31
		, texData :: [(Int, B.ByteString)]
		, texGenMipmap :: Bool
		}
	| Compressed
		{ texName :: String
		, texTarget :: TextureTarget
		, texLevel :: Int
		--, texFormat :: CompressionMethod
		, texWidth :: Int
		, texHeight :: Int
		, texSampler :: Sampler
		, texUnit :: Int -- Maybe Int 0..31
		, texData :: [(Int, B.ByteString)]
		, texGenMipmap :: Bool
		}
	| Texture'
		{
		  texName :: String
		, texSampler :: Sampler
		, texRef :: TextureRef
		, texUnit :: Int
		}

data TextureTarget =
	  Tex2D
	| CubeMapPositiveX
	| CubeMapPositiveY
	| CubeMapPositiveZ
	| CubeMapNegativeX
	| CubeMapNegativeY
	| CubeMapNegativeZ
	| Tex3D -- ^ introduced in ES 3.0
	| Tex2DArray -- ^ introduced in ES 3.0
	deriving (Show, Eq)

instance Marshal TextureTarget where
	marshal x = case x of
		Tex2D          -> 0x0DE1
		TexCubeMap     -> 0x8513
		TexCubeMapPosX -> 0x8515
		TexCubeMapPosY -> 0x8517
		TexCubeMapPosZ -> 0x8519
		TexCubeMapNegX -> 0x8516
		TexCubeMapNegY -> 0x8518
		TexCubeMapNegZ -> 0x851A
		Tex3D          -> 0x806F
		Tex2DArray     -> 0x8C1A
-}




{-
data FramebufferTarget = Framebuffer
data FramebufferTarget' = Framebuffer'
                        | DrawFramebuffer -- ^ ES 3.0
                        | ReadFramebuffer -- ^ ES 3.0
instance BindTarget	FramebufferId FramebufferTarget
instance Marshal FramebufferTarget where
	marshal Framebuffer = 0x8D40

data RenderbufferTarget = Renderbuffer
instance BindTarget	RenderbufferId RenderbufferTarget
instance Marshal RenderbufferTarget where
	marshal Renderbuffer = 0x8D40

-- | ES 3.0
data QueryTarget = AnySamplesPassed | AnySamplesPassedConservative
instance BindTarget	QueryId QueryTarget
instance Marshal QueryTarget where
	marshal AnySamplesPassed = 0x8C2F
	marshal AnySamplesPassedConservative = 0x8D6A

-- | ES 3.0
data TransformFeedbackTarget = TransformFeedback
instance BindTarget	TransformFeedback TransformFeedbackTarget
instance Marshal TransformFeedbackTarget where
	marshal TransformFeedback = 0x8E22

useProgram :: ProgramId -> IO ()
useProgram (ProgramId p) = glUseProgram p

deleteProgram :: ProgramId -> IO ()
deleteProgram (ProgramId p) = glDeleteProgram p

	DrawTextureExt -- XXX test: may not work with 2.0+ context.
		:: !TextureObj
		-> !Int32
		-> !Int32
		-> !Int32
		-> !Int32
		-> !Int32
		-> !Int32
		-> !Int32
		-> !Int32
		-> !Int32
		-> GLCall GLError
	-- XXX integrate as buffer operations?
	ViewPort -- ^ Cliping rendering area. The origin is left-bottom.
		:: !Int32 -- ^ x
		-> !Int32 -- ^ y
		-> !Int32 -- ^ w
		-> !Int32 -- ^ h
		-> GLCall GLError
BeginTransformFeedback :: TransformFeedback -> DrawMode -> GLCall ()
EndTransformFeedback :: GLCall ()

-- Performance Memo
-- WRONG -> L1Cache is fast. RIGHT -> Main memory is slow.
-- 
-- data Foo = Foo Char Int deriving (Data,Typeable,Show)
-- will add 20KB per derivation. -O make things worse: 36KB
-- Conclusion: Avoid using meta functions in busy loop

type GLName = CString -- eliminate hungry [Char]s. +rewrite rule.

-- | Read the OpenGL ES man pages for detail.
-- <http://www.khronos.org/opengles/sdk/docs/man31/>
-- <http://www.khronos.org/files/opengles3-quick-reference-card.pdf>
	-- GenFramebuffer :: GLCall FBO
	-- ModFramebuffer :: GLCall GLError
	-- GenRenderbuffer :: RenderbufferUsage -> GLCall RBO
	-- SetFramebuffer in GraphicState?
	-- BeginQuery :: QueryType -> GLCall GLError
	-- EndQuery :: GLCall ()
-}