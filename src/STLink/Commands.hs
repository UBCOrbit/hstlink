{-|
Module      : STLink.Commands
Description : Commands for talking to an STLink dongle
Copyright   : (c) UBC Orbit
Maintainer  : sam.schweigel@ubcorbit.com

All the raw commands that an STLink dongle understands can be found
here, though high-level wrappers for them are elsewhere.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE LambdaCase          #-}

module STLink.Commands
  ( getVersion
  , ResponseVersion(..)
  , getCurrentMode
  , STLinkMode(..)
  , getCurrentVoltage
  ) where

import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString.Lazy as BL

import           Control.Applicative

import           STLink.Driver

-- | Returns the smallests number with 'n' high bits.
bitsOn :: (Num a, Bits a) => Int -> a
bitsOn n = f n 0
  where
    f 0 m = shiftR m 1
    f l m = f (l - 1) (shift (m .|. 1) 1)

-- | Selects a bitfield at bits position 'p' (couting from the LSB)
-- that is 'w' bits wide.
--
-- >>> selectBits (3, 2) 0x789
-- 1
selectBits :: (Integral a, Bits a, Integral n) => (Int, Int) -> a -> n
selectBits (p, w) n = fromIntegral $ shiftR n p .&. bitsOn w

-- | This typeclass handles "inward"-bound STLink commands, by
-- associating the command's encoding with the response's encoding, as
-- well as the response's length.
--
-- 'c' must be the datatype of the *command*, not the response.
class InCommand c where
  -- | The associated response type for this command.
  type InResponse c :: *
  -- | The binary (put) encoding for the command.
  inCommandEncoding :: c -> Put
  -- | The expected size of the response to this command.
  inResponseSize :: Int
  -- | The binary (get) encoding for the response.
  inResponseEncoding :: Get (InResponse c)
  -- | A reasonable default implementation of issuing an "inward"
  -- command.  It's unlikely instances of this class will have to
  -- provide an implementation of this.
  runInCommand :: c -> STLink (InResponse c)
  runInCommand c =
    STLink $ \h ->
      runGet (inResponseEncoding @c) . BL.fromStrict <$>
      inCommand
        h
        (BL.toStrict . runPut . inCommandEncoding $ c)
        (inResponseSize @c)

data CommandVersion =
  CommandVersion
  deriving (Show, Eq)

-- | The STLink version reported by the dongle.  The response also
-- contians the VID and PID of the USB interface, but this is ignored
-- since it is redundant.
data ResponseVersion = ResponseVersion
  { versionStlink :: Int -- ^ The STLink protocol version this dongle
                         -- understands.
  , versionJtag   :: Int -- ^ Which JTAG revision the dongle is
                         -- capable of.
  , versionSwim   :: Int -- ^ Purpose unclear.
  } deriving (Show, Eq)

instance InCommand CommandVersion where
  type InResponse CommandVersion = ResponseVersion
  inCommandEncoding _ = putWord8 0xf1
  inResponseSize = 6
  inResponseEncoding = do
    n <- getWord16le
    pure $
      ResponseVersion
        (selectBits (12, 4) n)
        (selectBits (6, 6) n)
        (selectBits (0, 6) n)

-- | Get the version of an STLink dongle.
getVersion :: STLink ResponseVersion
getVersion = runInCommand CommandVersion

data CommandGetMode =
  CommandGetMode
  deriving (Show, Eq)

data STLinkMode
  = ModeDFU
  | ModeMass
  | ModeDebug
  | ModeSWIM
  | ModeBootloader
  deriving (Show, Eq)

instance InCommand CommandGetMode where
  type InResponse CommandGetMode = STLinkMode
  inCommandEncoding _ = putWord8 0xf5
  inResponseSize = 2
  inResponseEncoding = getWord8 >>= \case
    0x0 -> pure ModeDFU
    0x1 -> pure ModeMass
    0x2 -> pure ModeDebug
    0x3 -> pure ModeSWIM
    0x4 -> pure ModeBootloader
    _ -> empty

getCurrentMode :: STLink STLinkMode
getCurrentMode = runInCommand CommandGetMode

data CommandGetVoltage =
  CommandGetVoltage
  deriving (Show, Eq)

instance InCommand CommandGetVoltage where
  type InResponse CommandGetVoltage = Float
  inCommandEncoding _ = putWord8 0xf7
  inResponseSize = 8
  inResponseEncoding = do
    divisor <- fromIntegral <$> getInt32le
    coef <- fromIntegral <$> getInt32le
    pure $ 2 * coef * (1.2 / divisor)

-- | Get the voltage on the output of the STLink regulator.  (Should
-- be ~3.3V).
getCurrentVoltage :: STLink Float
getCurrentVoltage = runInCommand CommandGetVoltage

