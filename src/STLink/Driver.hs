module STLink.Driver
  ( openBoard
  ) where

import Control.Monad
import System.USB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Data.Binary
import Data.Binary.Get
import Data.Bits

import STLink.Detection

data ResponseVersion
  = ResponseVersion
    { versionStlink :: Int
    , versionJtag :: Int
    , versionSwim :: Int
    }
  deriving (Show, Eq)

instance Binary ResponseVersion where
  put = undefined
  get = do
    n <- getWord16le
    pure $ ResponseVersion
      (selectBits (12, 4) n)
      (selectBits (6, 6) n)
      (selectBits (0, 6) n)

bitsOn :: (Num a, Bits a) => Int -> a
bitsOn n = f n 0
  where
    f 0 m = shiftR m 1
    f n m = f (n-1) (shift (m .|. 1) 1)

selectBits :: (Integral a, Bits a, Integral n) => (Int, Int) -> a -> n
selectBits (p, w) n = fromIntegral $ shiftR n p .&. bitsOn w

getVersion :: DeviceHandle -> IO ResponseVersion
getVersion h =
  decode . BL.fromStrict <$> inCommand h "\xf1" 6

inCommand :: DeviceHandle -> BS.ByteString -> Size -> IO BS.ByteString
inCommand h c s = do
  -- send the command and make sure it was sucessful
  (size, sendstatus) <- writeBulk h (EndpointAddress 1 Out) c 1000
  guard $ size == BS.length c && sendstatus == Completed

  -- receive the response, and ensure it's what we expected
  (d, recstatus) <- readBulk h (EndpointAddress 1 In) s 1000
  guard $ s == BS.length d && recstatus == Completed
  pure d

outCommand :: DeviceHandle -> BS.ByteString -> BS.ByteString -> IO BS.ByteString
outCommand h c d = do
  -- send the command and make sure it was sucessful
  (sendsize, sendstatus) <- writeBulk h (EndpointAddress 1 Out) c 1000
  guard $ sendsize == BS.length c && sendstatus == Completed

  -- send the data
  (recsize, recstatus) <- writeBulk h (EndpointAddress 1 Out) d 1000
  guard $ recsize == BS.length d && recstatus == Completed
  pure d

openBoard :: IO ()
openBoard = pickBoard >>=
  maybe (pure ()) claimBoard

claimBoard :: Device -> IO ()
claimBoard d = withDeviceHandle d $ \h -> do
  withDetachedKernelDriver h 0 $ do
    withClaimedInterface h 0 $ do
      getVersion h >>= print
