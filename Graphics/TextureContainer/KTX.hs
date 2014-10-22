{-# LANGUAGE RecordWildCards #-}
-- | Khronos Texture Container Format
-- 
-- See also <http://www.khronos.org/opengles/sdk/tools/KTX/file_format_spec/>
module Graphics.TextureContainer.KTX where
import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Packer
import Data.Word

data Ktx = Ktx
	{ ktxName :: FilePath -- ^ for debugging
	, ktxContent :: B.ByteString -- ^ holding the original ForeignPtr.
	, ktxGlType :: Word32
	, ktxGlTypeSize :: Word32
	, ktxGlFormat :: Word32
	, ktxGlInternalFormat :: Word32
	, ktxGlBaseInternalFormat :: Word32
	, ktxPixelWidth :: Word32
	, ktxPixelHeight :: Word32
	, ktxPixelDepth :: Word32
	, ktxNumElems :: Word32
	, ktxNumFaces :: Word32
	, ktxNumMipLevels :: Word32
	, ktxMap :: [(B.ByteString, B.ByteString)] -- ^ (utf8, any)
	-- Note that if a value is utf8, it is NULL terminated.
	, ktxImage :: [[B.ByteString]]
	}  deriving Show

unpackKtx :: FilePath -> B.ByteString -> Unpacking Ktx
unpackKtx name orig = do
	let w = getWord32
	
	-- '«', 'K', 'T', 'X', ' ', '1', '1', '»', '\r', '\n', '\x1A', '\n'
	(0x58544BAB, 0xBB313120, 0x0A1A0A0D) <- (,,) <$> w <*> w <*> w
	
	-- Endianness
	-- Assuming Big-endian is a loser of history, just ignore it.
	-- Note: All modern platforms (Android, iOS, Windows, ...)
	-- runs Little-endian nevertheless the processor is bi-endian.
	0x04030201 <- getWord32
	
	ktx <- Ktx name orig <$> w <*> w <*> w <*> w <*> w <*> w
			<*> w <*> w <*> w <*> w <*> w

	bytesOfKeyValueData <- getWord32
	let getKVP 0 = return []
	    getKVP i = do
		keyAndValueByteSize <- getWord32
		x <- getBytes (fromIntegral keyAndValueByteSize)
		let padding = 3 - (keyAndValueByteSize + 3) `mod` 4
		unpackSkip (fromIntegral padding)
		xs <- getKVP (i - keyAndValueByteSize - padding)
		return (x:xs)
	kvp <- map (B.breakByte 0) <$> getKVP bytesOfKeyValueData
	let Ktx{..} = ktx kvp []

	imgs <- forM [1..max 1 ktxNumMipLevels] $ \_ -> do
		imageSize <- getWord32
		forM [1..max 1 ktxNumFaces] $ \_ -> do
			img <- getBytes (fromIntegral imageSize)
			unpackSkip $ fromIntegral (3 - (imageSize + 3) `mod` 4)
			return img

	return $ ktx kvp imgs

ktxFromFile :: FilePath -> IO Ktx
ktxFromFile path = B.readFile path >>= return . readKtx path

readKtx :: FilePath -> B.ByteString -> Ktx
readKtx path bs = runUnpacking (unpackKtx path bs) bs

tryKtx :: FilePath -> B.ByteString -> Either SomeException Ktx
tryKtx path bs = tryUnpacking (unpackKtx path bs) bs

{-
type MipmapData = [Face or ArrayElements]
type ArrayElements = B.ByteString
type Face = B.ByteString

Byte[12] identifier
UInt32 endianness
UInt32 glType
UInt32 glTypeSize
UInt32 glFormat
Uint32 glInternalFormat
Uint32 glBaseInternalFormat
UInt32 pixelWidth
UInt32 pixelHeight
UInt32 pixelDepth
UInt32 numberOfArrayElements
UInt32 numberOfFaces
UInt32 numberOfMipmapLevels
UInt32 bytesOfKeyValueData
  
for each keyValuePair that fits in bytesOfKeyValueData
    UInt32   keyAndValueByteSize
    Byte     keyAndValue[keyAndValueByteSize]
    Byte     valuePadding[3 - ((keyAndValueByteSize + 3) % 4)]
end
  
for each mipmap_level in numberOfMipmapLevels*
    UInt32 imageSize; 
    for each array_element in numberOfArrayElements*
       for each face in numberOfFaces
           for each z_slice in pixelDepth*
               for each row or row_of_blocks in pixelHeight*
                   for each pixel or block_of_pixels in pixelWidth
                       Byte data[format-specific-number-of-bytes]**
                   end
               end
           end
           Byte cubePadding[0-3]
       end
    end
    Byte mipPadding[3 - ((imageSize + 3) % 4)]
end

* Replace with 1 if this field is 0.

** Uncompressed texture data matches a GL_UNPACK_ALIGNMENT of 4.
-}