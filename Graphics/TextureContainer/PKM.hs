{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
-- | Ericsson ETC1/ETC2/EAC Texture Container Format
module Graphics.TextureContainer.PKM where
import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Packer
import Data.Word
import Graphics.TextureContainer.KTX

data Pkm = Pkm
	{ pkmName :: FilePath
	, pkmContent :: B.ByteString
	, pkmVersion :: Word16
	, pkmType :: Word16
	, pkmWidth :: Word16
	, pkmHeight :: Word16
	, pkmActiveW :: Word16
	, pkmActiveH :: Word16
	, pkmImage :: B.ByteString
	} deriving Show

unpackPkm :: FilePath -> B.ByteString -> Unpacking Pkm
unpackPkm name orig = do
	-- 'PKM '
	0x204d4b50 <- getWord32
	
	let w = getWord16BE
	pkm <- Pkm name orig <$> w <*> w <*> w <*> w <*> w <*> w
	-- XXX calc size from pkmVersion, pkmType
	let size = pkmActiveW (pkm "") * pkmActiveH (pkm "") `div` 2
	pkm <$> getBytes (fromIntegral size)

pkmToKtx :: Pkm -> Ktx
pkmToKtx Pkm{..} = Ktx
	{ ktxName = pkmName
	, ktxContent = pkmContent
	, ktxGlType = 0
	, ktxGlTypeSize = 1
	, ktxGlFormat = 0
	, ktxGlInternalFormat = fromPkmType pkmVersion pkmType
	, ktxGlBaseInternalFormat = 0
	, ktxPixelWidth = fromIntegral pkmWidth
	, ktxPixelHeight = fromIntegral pkmHeight
	, ktxPixelDepth = 0
	, ktxNumElems = 0
	, ktxNumFaces = 1
	, ktxNumMipLevels = 1
	, ktxMap = []
	, ktxImage = [[pkmImage]]
	}
	where
		-- 12592 == '1''0', 12848 == '2''0'
		fromPkmType 12592 0 = 36196 -- ?
		fromPkmType 12592 1 = undefined
		fromPkmType 12592 2 = undefined
		fromPkmType 12592 3 = undefined
		fromPkmType 12848 0 = undefined
		fromPkmType 12848 1 = undefined
		fromPkmType 12848 2 = undefined
		fromPkmType 12848 3 = undefined
		fromPkmType 12848 4 = undefined
		fromPkmType 12848 5 = undefined
		fromPkmType 12848 6 = undefined
		fromPkmType 12848 7 = undefined
		fromPkmType 12848 8 = undefined

fromPkmFile path = B.readFile path >>= return . readPkm path

readPkm :: FilePath -> B.ByteString -> Pkm
readPkm path bs = runUnpacking (unpackPkm path bs) bs

{-

// 16bytes big-endian
unsigned int32     magic;     // 'PKM '
unsigned short16   version;   // '10' or '20'
unsigned short16   type;      // See below
unsigned short16   width;     // 
unsigned short16   height;    // 
unsigned short16   active_w;  // 
unsigned short16   active_h;  // 

version 1.0
  type: 0=ETC1-RGB, 1=ETC1-RGBA, 2=ETC1-RGB-MIP, 3=ETC1-RGBA-MIP
version 2.0
  type: 0=ETC1_RGB, 1=ETC2_RGB, 2=ETC2_RGBA_OLD, 3=ETC2_RGBA, 4=ETC2_RGBA1, 5=ETC2_R, 6=ETC2_RG, 7=ETC2_SIGNED_R, 8=ETC2_SIGNED_R

-}