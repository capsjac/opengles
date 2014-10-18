{-# LANGUAGE RecordWildCards #-}
module Graphics.OpenGLES.Texture (
  -- * Texture
  Texture,
  glLoadKtx,
  glLoadKtxFile,

  texSlot,

  -- * Sampler
  setSampler,
  Sampler(..),
  
  MagFilter,
  magNearest,
  magLinear,
  
  MinFilter,
  minNearest,
  minLinear,
  nearestMipmapNearest,
  linearMipmapNearest,
  nearestMipmapLinear,
  linearMipmapLinear,
  
  WrapMode,
  tiledRepeat,
  clampToEdge,
  mirroredRepeat
  ) where
import Control.Applicative
import Control.Monad
import qualified Data.ByteString as B
import Data.Int
import Data.Word
import Data.IORef
import Graphics.OpenGLES.Base
import Graphics.OpenGLES.Env
import Graphics.OpenGLES.Internal
import Graphics.OpenGLES.PixelFormat
import Graphics.TextureContainer.KTX
import Foreign.Ptr (castPtr)

-- glo, target, ktx
data Texture a = Texture GLenum (IORef Ktx) GLO
-- XXX Texture DoubleBufferring

newtype Texture3D a = Texture3D (Texture a)
newtype CubeMap a = CubeMap (Texture a)
newtype Tex2DArray a = Tex2DArray (Texture a)

-- Iso
--class Tex a where
--	fromTexture :: Texture b -> a b
--instance Tex Texture where
--	fromTexture = id
--instance Tex Texture3D where
--	fromTexture = Texture3D
--instance Tex CubeMap where
--	fromTexture = CubeMap
--instance Tex Tex2DArray where
--	fromTexture = Tex2DArray

texSlot :: Word32 -> Texture a -> GL ()
texSlot slot (Texture target _ glo) = do
	tex <- getObjId glo
	glActiveTexture (0x84C0 + slot) -- GL_TEXTURE_0 + slot
	glBindTexture target tex

data TextureData =
	  PlainTexture
	| PVRTC -- PowerVR SGX(iOS), Samsung S5PC110(Galaxy S/Tab)
	| ATITC -- ATI Imageon, Qualcomm Adreno
	| S3TC -- NVIDIA Tegra2, ZiiLabs ZMS-08 HD
	| ETC -- Android, ARM Mali
	-- .| _3Dc -- ATI, NVidia
	-- .| Palette

-- | Load a GL Texture object from Ktx texture container.
-- See <https://github.com/KhronosGroup/KTX/blob/master/lib/loader.c>
-- TODO: reject 2DArray/3D texture if unsupported
glLoadKtx :: Maybe (Texture a) -> Ktx -> GL (Texture a)
glLoadKtx oldtex ktx@Ktx{..} = do
	--putStrLn.show $ ktx
	let newTexture target = case oldtex of
		Just (Texture _ ref glo) -> do
			writeIORef ref ktx
			glBindTexture target =<< getObjId glo
			return (Texture target ref glo)
		Nothing -> Texture target
			<$> newIORef ktx
			<*> newGLO glGenTextures glDeleteTextures (glBindTexture target)
	case checkKtx ktx of
		Left msg -> glLog (msg ++ ": " ++ ktxName) >> newTexture 0
		Right (comp, target, genmip) -> do
			tex <- newTexture target
			-- ES2 compat;
			let fmt = if comp || sizedFormats
				then if ktxGlInternalFormat == 0x8D64 && hasETC2 -- ETC1_RGB8_OES
					then 0x9274 -- GL_COMPRESSED_RGB8_ETC2
					else ktxGlInternalFormat
				else ktxGlBaseInternalFormat
			putStrLn . show $ (comp, target,genmip, ktxGlInternalFormat, hasETC2, fmt)
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

glLoadKtxFile :: FilePath -> GL (Texture a)
glLoadKtxFile path = ktxFromFile path >>= glLoadKtx Nothing

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
hasETC2 = hasES3
hasPVRTC = hasExt "GL_IMG_texture_compression_pvrtc"
hasATITC = hasExt "GL_ATI_texture_compression_atitc"
hasS3TC = hasExt "GL_EXT_texture_compression_s3tc"

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
-- (specify 1.0 to disable anisotropy filter), (Fallback) Mag and Min filters).
-- 
-- When /EXT_texture_filter_anisotropic/ is not supported, fallback filters
-- are used instead.
data Sampler =
	Sampler (WrapMode, WrapMode, Maybe WrapMode) Float (MagFilter, MinFilter)

newtype MagFilter = MagFilter Int32
magNearest = MagFilter 0x2600
magLinear = MagFilter 0x2601

-- TODO NoMipmap => use minLinear instead / forceGenMip?
newtype MinFilter = MinFilter Int32
minNearest = MinFilter 0x2600
minLinear = MinFilter 0x2601
nearestMipmapNearest = MinFilter 0x2700
linearMipmapNearest = MinFilter 0x2701
nearestMipmapLinear = MinFilter 0x2702
linearMipmapLinear = MinFilter 0x2703

-- TODO: NPOT && not ClampToEdge && not (hasES3 || hasExt "GL_OES_texture_npot") => error
newtype WrapMode = WrapMode Int32
tiledRepeat = WrapMode 0x2901
clampToEdge = WrapMode 0x812F
mirroredRepeat = WrapMode 0x8370

setSampler :: Texture a -> Sampler -> GL ()
setSampler (Texture target _ glo) (Sampler (WrapMode s, WrapMode t, r) a
		(MagFilter g, MinFilter n)) = do
	tex <- getObjId glo
	glBindTexture target tex
	glTexParameteri target 0x2802 s
	glTexParameteri target 0x2803 t
	maybe (return ()) (\(WrapMode x) -> glTexParameteri target 0x8072 x) r
	--if a /= 1.0 && hasExt "GL_EXT_texture_filter_anisotropic" then
	-- GL_TEXTURE_MAX_ANISOTROPY_EXT
	glTexParameterf target 0x84FE a
	glTexParameteri target 0x2800 g
	glTexParameteri target 0x2801 n

--setLOD :: Texture -> Int32 -> Int32 -> GL ()
--setTexCompFunc :: Texture ->  ->  -> GL ()

-- mipmaps, compressed, fallbacks
-- [comptex, fallback]
-- glm basepath = /data/app.name/assets/...
-- glm preffered compression type (filename suffiex)
---- iOS: PVRTC
---- Android with Alpha: PVRTC+ATITC+S3TC+Uncompressed
---- Android without Alpha: ETC1
---- PC: S3TC + Uncompressed
--glm detect compression support
-- record Gen/Del/Draw* calls and responces
-- prefferedformat == "etc1" like.
-- texture from file "name" [path..] genMipmap?flag
--Utils.hs shrinked triangles, 
--  Update[Sub]Texture,UpdateVertex(Buffer)
-- "texname" [tex1,tex2,...] auto select
-- texture atlas managment

