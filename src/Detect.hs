module Detect
  ( pickBoard
  ) where

import System.USB

import qualified Data.Vector as V
import Data.Word
import Data.Functor

import qualified Data.Text as T
import Text.PrettyPrint.Boxes

import Numeric (showHex)

-- | All STLINK boards have this vendor ID.
stVendorId :: Word16
stVendorId = 0x0483

getDeviceAndDesc :: Device -> IO (Device, DeviceDesc)
getDeviceAndDesc d = (d,) <$> getDeviceDesc d

toHex :: (Show n, Integral n) => n -> String
toHex x = showHex x ""

getStrUSB :: (DeviceDesc -> Maybe StrIx) -> (Device, DeviceDesc) -> IO T.Text
getStrUSB f (d, dd) = withDeviceHandle d $ \h ->
  case f dd of
    Just i -> getStrDescFirstLang h i 256
    Nothing -> pure ""

-- | Print a pretty table of available boards we detected.
printBoards :: [(Device, DeviceDesc)] -> IO ()
printBoards ds = do
  names <- traverse (getStrUSB deviceProductStrIx) ds
  serials <- traverse (getStrUSB deviceSerialNumberStrIx) ds

  let numbox = vcat right $ "num" :
        (text . (\t -> "[" ++ t ++ "]") . show <$> [1..length ds])
      productbox = vcat right $
        "id" : (text . toHex . deviceProductId . snd <$> ds)
      namebox = vcat left $ "name" : (text . T.unpack <$> names)
      serialbox = vcat left $ "serial" : (text . T.unpack <$> serials)

  printBox $ numbox <+> productbox <+> namebox <+> serialbox

-- | Find every ST board connected to the USB controller.
findStMicros :: IO (V.Vector (Device, DeviceDesc))
findStMicros = do
  -- set up a USB context and get all connected devices
  ctx <- newCtx
  devs <- getDevices ctx

  -- get a vector of device descriptors, filter out ST ones
  descs <- traverse getDeviceAndDesc devs
  pure $ V.filter
    ((== stVendorId) . deviceVendorId . snd)
    descs

-- | Ask the user to select one of the available board, or fail if
-- none are available.
pickBoard :: IO (Maybe Device)
pickBoard = do
  micros <- findStMicros
  printBoards (V.toList micros)
  case length micros of
    0 -> putStrLn "No boards detected!" $>
         Nothing
    1 -> putStrLn "Auto-selecting board 1." $>
         Just (fst . V.head $ micros)
    _ -> do
      putStr "Select board: "
      num <- read <$> getLine
      pure $ fst <$> V.indexM micros (num - 1)
