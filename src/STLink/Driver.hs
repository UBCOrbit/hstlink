{-|
Module      : STLink.Driver
Description : Low-level USB driver for STLink dongles
Copyright   : (c) UBC Orbit
Maintainer  : sam.schwiegel@ubcorbit.com

Most users shouldn't have to import this module, since the important
bits are re-exported elsewhere.
-}
{-# LANGUAGE LambdaCase #-}

module STLink.Driver
  ( inCommand
  , outCommand
  , withAutoBoard
  , withBoard
  , STLink(STLink)
  ) where

import           Control.Monad.Except
import qualified Data.ByteString      as BS
import           STLink.Detection
import           System.USB

-- | Utility function that throws an error 'e' in the 'ExceptT' monad
-- if the condition is met.
eguard :: Monad m => e -> Bool -> ExceptT e m ()
eguard e True  = throwError e
eguard _ False = pure ()

-- | Low-level command that formats an "inward" command the way the
-- STLink dongle likes it:
--
-- @
-- USB_BULK out \<up to 16 bytes of commands\>
-- USB_BULK in \<expected response length\>
-- @
--
-- Since USB is a host-oriented protocol, we have to know in advance
-- how many bytes to read from the device's buffer.
inCommand ::
     DeviceHandle -> BS.ByteString -> Size -> ExceptT String IO BS.ByteString
inCommand h c s
  -- send the command and make sure it was sucessful
 = do
  (size, sendStatus) <- liftIO $ writeBulk h (EndpointAddress 1 Out) c 1000
  eguard "sent command truncated" $ size /= BS.length c
  eguard "send timed out" $ sendStatus /= Completed
  -- receive the response, and ensure it's what we expected
  (d, recStatus) <- liftIO $ readBulk h (EndpointAddress 1 In) s 1000
  eguard "response truncated" $ s /= BS.length d
  eguard "receive timed out" $ recStatus /= Completed
  pure d

-- | Low-level command that formats an "outward" command the way the
-- STLink dongle likes it:
--
-- @
-- USB_BULK out \<up to 16 bytes of commands\>
-- USB_BULK out \<outward-bound data\>
-- @
outCommand ::
     DeviceHandle -> BS.ByteString -> BS.ByteString -> ExceptT String IO ()
outCommand h c d
  -- send the command and make sure it was sucessful
 = do
  (sendSize, sendStatus) <- liftIO $ writeBulk h (EndpointAddress 1 Out) c 1000
  eguard "sent command truncated" $ sendSize /= BS.length c
  eguard "send command timed out" $ sendStatus /= Completed
  -- send the data
  (recSize, recStatus) <- liftIO $ writeBulk h (EndpointAddress 1 Out) d 1000
  eguard "sent data truncated" $ recSize /= BS.length d
  void $ eguard "send data timed out" $ recStatus /= Completed

-- | This monad lets you write effectful computations that communicate
-- with an STLink dongle, without explicitly specifying the dongle you
-- want to talk with for every command.  Generally, 'runSTLink
-- wouldn't be used directly since it takes a low-level 'DeviceHandle'
-- instead of a general 'Device'.  See 'withBoard' for a safe,
-- high-level way to run an 'STLink'.
newtype STLink a = STLink
  { runSTLink :: DeviceHandle -> ExceptT String IO a
  }

instance Functor STLink where
  fmap f (STLink m) = STLink $ fmap f . m

instance Applicative STLink where
  pure x = STLink $ const (pure x)
  (STLink f) <*> (STLink m) = STLink $ \h -> f h <*> m h

instance Monad STLink where
  (STLink m) >>= f = STLink $ \h -> m h >>= ($ h) . runSTLink . f

instance MonadIO STLink where
  liftIO m = STLink $ const (liftIO m)

-- | Automatically finds a board (and prompts the user to select one
-- if several are attached) and then runs the 'STLink' action with
-- that board.
withAutoBoard :: STLink a -> IO (Maybe a)
withAutoBoard s = pickBoard >>= maybe (pure Nothing) (`withBoard` s)

-- | Lets the user manually specify a USB 'Device' to execute the
-- 'STLink' action on.
withBoard :: Device -> STLink a -> IO (Maybe a)
withBoard d s =
  withDeviceHandle d $ \h ->
    withDetachedKernelDriver h 0 $
    withClaimedInterface h 0 $
    runExceptT (runSTLink s h) >>= \case
      Left e -> do
        liftIO . putStrLn $ "stlink driver error: " ++ e
        pure Nothing
      Right a -> pure $ Just a
