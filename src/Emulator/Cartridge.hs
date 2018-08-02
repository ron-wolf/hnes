module Emulator.Cartridge (
    Cartridge(..)
  , Mirror(..)
  , parse
) where

import           Control.Applicative         ((<*>))
import           Control.Monad               (liftM2)
import           Control.Monad.ST
import           Data.Bits
import qualified Data.ByteString             as BS
import           Data.Function               (on)
import           Data.IORef
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word
import           Emulator.Util               (sliceBS)
import           Prelude                     hiding (read)

-- data INesFileHeader = INesFileHeader {
--   format   :: Word8,
--   numPrg   :: Int,
--   numChr   :: Int,
--   control1 :: Int,
--   control2 :: Int,
--   numRam   :: Int
-- } deriving (Eq, Show)

data Mirror
  = MirrorHorizontal
  | MirrorVertical
  | MirrorSingle0
  | MirrorSingle1
  | MirrorFour
  deriving (Eq, Show, Enum)

data Cartridge = Cartridge {
  chrRom     :: VUM.MVector RealWorld Word8,
  prgRom     :: VUM.MVector RealWorld Word8,
  sram       :: VUM.MVector RealWorld Word8,
  mirror     :: IORef Mirror,
  mapperType :: Int
}

-- parseHeader :: BS.ByteString -> INesFileHeader
-- parseHeader bs = INesFileHeader
--   (fromIntegral $ BS.index bs 3)
--   (fromIntegral $ BS.index bs 4)
--   (fromIntegral $ BS.index bs 5)
--   (fromIntegral $ BS.index bs 6)
--   (fromIntegral $ BS.index bs 7)
--   (fromIntegral $ BS.index bs 8)

parse :: BS.ByteString -> IO Cartridge
parse bs = (liftM2 Cartridge `on` process) chrRom prgRom <*> sram <*> wrap mirrorV <*> pure mapper
  where
    process = VU.thaw . VU.fromList . BS.unpack
    wrap    = newIORef . (toEnum :: Int -> BS.ByteString)
    
    let prgOffset = numPrg * prgRomSize
        chrOffset = numChr * chrRomSize
    in  prgRom               = on sliceBS (headerSize +            ) 0 prgOffset bs
        chrRom | numChr /= 0 = on sliceBS (headerSize + prgOffset +) 0 chrOffset bs
               | otherwise   = BS.replicate chrRomSize 0
    
    sram    = VUM.replicate 0x2000 0
    mirrorV = (ctrl1 .&. 1 .|.) $ ((ctrl1 `shiftR` 3) .&. 1) `shiftR` 1
    mapper  = (ctrl1 `shiftR` 4) .|. (ctrl2 `shiftR` 4 `shiftL` 4)
    
    [numPrg, numChr, ctrl1, ctrl2] = map (BS.index bs) [4..7]

headerSize :: Int
headerSize = 0x10

prgRomSize :: Int
prgRomSize = 0x4000

chrRomSize :: Int
chrRomSize = 0x2000
