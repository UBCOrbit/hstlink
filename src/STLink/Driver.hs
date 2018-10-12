module STLink.Driver
  ( 
  ) where

import Control.Monad
import Control.Monad.Except

import System.USB

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Data.Binary
import Data.Binary.Get
import Data.Bits

import STLink.Detection

bitsOn :: (Num a, Bits a) => Int -> a
bitsOn n = f n 0
  where
    f 0 m = shiftR m 1
    f n m = f (n-1) (shift (m .|. 1) 1)

selectBits :: (Integral a, Bits a, Integral n) => (Int, Int) -> a -> n
selectBits (p, w) n = fromIntegral $ shiftR n p .&. bitsOn w

eguard :: Monad m => e -> Bool -> ExceptT e m ()
eguard e True = throwError e
eguard e False = pure ()

data OutCommand c d =
  OutCommand c d
  deriving (Show, Eq)

data InCommand c r =
  InCommand c Size
  deriving (Show, Eq)

decodeResp :: (MonadError String m, Binary r) => BS.ByteString -> m r
decodeResp d = either f s . decodeOrFail . BL.fromStrict $ d
  where
    f (_, _, m) = throwError $ "failed to decode response: " ++ m
    s (_, _, r) = pure r

inCommand :: (Binary c, Binary r) => DeviceHandle -> InCommand c r -> ExceptT String IO r
inCommand h (InCommand c s) = do
  -- send the command and make sure it was sucessful
  let cd = BL.toStrict . encode $ c
  (size, sendStatus) <- lift $ writeBulk h (EndpointAddress 1 Out) cd 1000
  eguard "sent command truncated" $ size /= BS.length cd
  eguard "send timed out" $ sendStatus /= Completed

  -- receive the response, and ensure it's what we expected
  (d, recStatus) <- lift $ readBulk h (EndpointAddress 1 In) s 1000
  eguard "response truncated" $ s /= BS.length d
  eguard "receive timed out" $ recStatus /= Completed
  decodeResp d

outCommand :: (Binary c, Binary d) => DeviceHandle -> OutCommand c d -> ExceptT String IO ()
outCommand h (OutCommand c d) = do
  -- send the command and make sure it was sucessful
  let cd = BL.toStrict . encode $ c
  (sendSize, sendStatus) <- lift $ writeBulk h (EndpointAddress 1 Out) cd 1000
  eguard "sent command truncated" $ sendSize /= BS.length cd
  eguard "send command timed out" $ sendStatus /= Completed

  -- send the data
  let dd = BL.toStrict . encode $ d
  (recSize, recStatus) <- lift $ writeBulk h (EndpointAddress 1 Out) dd 1000
  eguard "sent data truncated" $ recSize /= BS.length dd
  eguard "send data timed out" $ recStatus /= Completed

data CommandVersion
  = CommandVersion
  deriving (Show, Eq)

instance Binary CommandVersion where
  put _ = putWord8 0xf1
  get = undefined

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

type VersionCommand = InCommand CommandVersion ResponseVersion

newtype STLink a =
  STLink { runSTLink :: DeviceHandle -> ExceptT String IO a
         }

instance Functor STLink where
  fmap f (STLink m) = STLink $ fmap f . m

instance Applicative STLink where
  pure x = STLink $ const (pure x)
  (STLink f) <*> (STLink m) =
    STLink $ \h -> f h <*> m h

instance Monad STLink where
  (STLink m) >>= f =
    STLink $ \h -> (m h >>= ($ h) . runSTLink . f)

getVersion :: STLink ResponseVersion
getVersion = STLink $ \h -> inCommand h (InCommand CommandVersion 6)

withAutoBoard :: STLink a -> IO (Maybe a)
withAutoBoard s = pickBoard >>=
  maybe (pure Nothing) (\d -> withBoard d s)

withBoard :: Device -> STLink a -> IO (Maybe a)
withBoard d s = withDeviceHandle d $ \h ->
  withDetachedKernelDriver h 0 $
    withClaimedInterface h 0 $
      runExceptT (runSTLink s h) >>= \case
        Left e -> do
          putStrLn $ "stlink driver error: " ++ e
          pure Nothing
        Right a -> pure $ Just a
