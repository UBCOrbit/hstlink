{-|
Module      : STLink.Commands
Description : Commands for talking to an STLink dongle
Copyright   : (c) UBC Orbit
Maintainer  : sam.schweigel@ubcorbit.com

All the raw commands that an STLink dongle understands can be found
here, though high-level wrappers for them are elsewhere.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module STLink.Commands
  ( getVersion
  , ResponseVersion(..)
  , getCurrentMode
  , STLinkMode(..)
  , getCurrentVoltage
  , enterMode
  , DebugMode(..)
  , getStatus
  , DebugStatus
  , haltCore
  , runCore
  , stepCore
  , resetCore
  , readReg
  , writeReg
  , readMem
  , writeMem
  ) where

import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import           Data.Word
import qualified Data.ByteString as BS
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
  inResponseSize :: c -> Int
  -- | The binary (get) encoding for the response.
  inResponseEncoding :: c -> Get (InResponse c)
  -- | A reasonable default implementation of issuing an "inward"
  -- command.  It's unlikely instances of this class will have to
  -- provide an implementation of this.
  runInCommand :: c -> STLink (InResponse c)
  runInCommand c =
    STLink $ \h ->
      runGet (inResponseEncoding c) . BL.fromStrict <$>
      inCommand
        h
        (BL.toStrict . runPut . inCommandEncoding $ c)
        (inResponseSize c)

-- | This typeclass handles "outward"-bound STLink commands, by
-- associating the command's encoding with the operand's encoding.
class OutCommand c where
  -- | The associated operannd type for this command.
  type OutData c :: *
  -- | The binary (put) encoding for the command.
  outCommandEncoding :: c -> Put
  -- | The binary (put) encoding for the operand.
  outDataEncoding :: OutData c -> Put
  -- | Encode the command and operand, and issue the command.
  runOutCommand :: c -> OutData c -> STLink ()
  runOutCommand c d =
    STLink $ \h ->
      outCommand
        h
        (BL.toStrict . runPut . outCommandEncoding $ c)
        (BL.toStrict . runPut . outDataEncoding @c $ d)

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
  inResponseSize _ = 6
  inResponseEncoding _ = do
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

-- | Major mode for STLink, rather than the minor debug mode ('DebugMode').
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
  inResponseSize _ = 2
  inResponseEncoding _ =
    getWord8 >>= \case
      0x0 -> pure ModeDFU
      0x1 -> pure ModeMass
      0x2 -> pure ModeDebug
      0x3 -> pure ModeSWIM
      0x4 -> pure ModeBootloader
      _ -> empty

-- | Figure out what major mode the STLink dongle is operating in.
-- (Debug or otherwise.)
getCurrentMode :: STLink STLinkMode
getCurrentMode = runInCommand CommandGetMode

data CommandGetVoltage =
  CommandGetVoltage
  deriving (Show, Eq)

instance InCommand CommandGetVoltage where
  type InResponse CommandGetVoltage = Float
  inCommandEncoding _ = putWord8 0xf7
  inResponseSize _ = 8
  inResponseEncoding _ = do
    divisor <- fromIntegral <$> getInt32le
    coef <- fromIntegral <$> getInt32le
    pure $ 2 * coef * (1.2 / divisor)

-- | Get the voltage on the output of the STLink regulator.  (Should
-- be ~3.3V).
getCurrentVoltage :: STLink Float
getCurrentVoltage = runInCommand CommandGetVoltage

-- | Tell the STLink dongle that the following command is a debug
-- command.
putDebug :: Put -> Put
putDebug = (putWord8 0xf2 *>)

-- | Tell the STLink dongle that the following command is a SWIM
-- command.  (Unused)
putSWIM :: Put -> Put
putSWIM = (putWord8 0xf2 *>)

data CommandModeEnter =
  CommandModeEnter DebugMode
  deriving (Show, Eq)

-- | Which debugging interface the dongle with interface with.  Prefer
-- SWD when possible.
data DebugMode
  = DebugJTAG
  | DebugSWD
  | DebugSWIM
  deriving (Show, Eq)

instance InCommand CommandModeEnter where
  type InResponse CommandModeEnter = ()
  inCommandEncoding (CommandModeEnter m) =
    case m of
      DebugJTAG -> putDebug $ putWord16be 0x3000
      DebugSWD  -> putDebug $ putWord16be 0x30a3
      DebugSWIM -> putSWIM $ putWord8 0x00
  inResponseSize _ = 2
  inResponseEncoding _ = pure ()

-- | Leave either DFU or Mass mode and enter one of the three debug
-- modes.
enterMode :: DebugMode -> STLink ()
enterMode = runInCommand . CommandModeEnter

data CommandGetStatus =
  CommandGetStatus
  deriving (Show, Eq)

-- | Determines if the CPU is running or halted (through debug
-- commands).
data DebugStatus
  = DebugStatusRunning
  | DebugStatusHalted
  deriving (Show, Eq)

instance InCommand CommandGetStatus where
  type InResponse CommandGetStatus = DebugStatus
  inCommandEncoding CommandGetStatus = putDebug $ putWord8 0x01
  inResponseSize _ = 2
  inResponseEncoding _ =
    getWord8 >>= \case
      0x80 -> pure DebugStatusRunning
      0x81 -> pure DebugStatusHalted
      _ -> empty

-- | Get the current state of the CPU.  You must be in SWD or JTAG
-- mode to issue this command.
getStatus :: STLink DebugStatus
getStatus = runInCommand CommandGetStatus

-- | How to alter the execution of the micro.
data CommandExecution
  = ExecuteRun
  | ExecuteHalt
  | ExecuteReset
  | ExecuteStep
  deriving (Eq, Show)

instance InCommand CommandExecution where
  type InResponse CommandExecution = ()
  inCommandEncoding s =
    case s of
      ExecuteRun   -> putDebug $ putWord8 0x09
      ExecuteHalt  -> putDebug $ putWord8 0x02
      ExecuteReset -> putWord8 0x32
      ExecuteStep  -> putDebug $ putWord8 0x0a
  inResponseSize _ = 2
  inResponseEncoding _ = pure ()

-- | Stop execution on the microcontroller.
haltCore :: STLink ()
haltCore = runInCommand ExecuteHalt

-- | Continue execution on the microcontroller.
runCore :: STLink ()
runCore = runInCommand ExecuteRun

-- | Reset the microcontroller.
resetCore :: STLink ()
resetCore = runInCommand ExecuteReset

-- | Step one instruction on the microcontroller.
stepCore :: STLink ()
stepCore = runInCommand ExecuteStep

type Reg = Word8

data CommandReadReg =
  CommandReadReg Reg
  deriving (Eq, Show)

instance InCommand CommandReadReg where
  type InResponse CommandReadReg = Word32
  inCommandEncoding (CommandReadReg n) =
    putDebug $ putWord8 0x33 *> putWord8 n
  inResponseSize _ = 8
  inResponseEncoding _ = getWord32le *> getWord32le
  
-- | Read the specified real register. (Debug registers are not
-- accessible through this command).
readReg :: Reg -> STLink Word32
readReg = runInCommand . CommandReadReg

data CommandWriteReg =
  CommandWriteReg Reg Word32

instance InCommand CommandWriteReg where
  type InResponse CommandWriteReg = ()
  inCommandEncoding (CommandWriteReg n v) =
    putDebug $ putWord8 0x34 *> putWord8 n *> putWord32le v
  inResponseSize _ = 2
  inResponseEncoding _ = pure ()

-- | Write a 'Word32' to the specified 'Reg'.
writeReg :: Reg -> Word32 -> STLink ()
writeReg n v = runInCommand $ CommandWriteReg n v

data CommandReadMem =
  CommandReadMem Word32 Word16
  deriving (Eq, Show)

instance InCommand CommandReadMem where
  type InResponse CommandReadMem = BS.ByteString
  inCommandEncoding (CommandReadMem a l) = putDebug $ do
    putWord8 0x0c
    putWord32le a
    putWord16le l
  inResponseSize (CommandReadMem _ l) = fromIntegral l
  inResponseEncoding (CommandReadMem _ l) = getByteString (fromIntegral l)

-- | Peeks 'l' bytes of memory at address 'a'.
--
-- A maximum of 64 bytes can be read in one command.
readMem :: Word32 -> Word16 -> STLink BS.ByteString
readMem a l = runInCommand (CommandReadMem a l)

data CommandWriteMem =
  CommandWriteMem Word32 Word16
  deriving (Eq, Show)

instance OutCommand CommandWriteMem where
  type OutData CommandWriteMem = BS.ByteString
  outCommandEncoding (CommandWriteMem a l) = putDebug $ do
    putWord8 0x0d
    putWord32le a
    putWord16le l
  outDataEncoding = putByteString

-- | Writes a 'BS.ByteString' into the location in memory specified by
-- 'a'.
--
-- A maximum of 64 bytes can be written in one command.
writeMem :: Word32 -> BS.ByteString -> STLink ()
writeMem a d = runOutCommand
  (CommandWriteMem a (fromIntegral $ BS.length d)) d
