{-|
Module      : STLink.Detection
Description : Auto-detects and selects STLink dongles
Copyright   : (c) UBC Orbit
Maintainer  : sam.schweigel@ubcorbit.com

This module can detect attached STLink dongles, and has some fnuctions
for pretty-printing a table of detected devices, as well as prompting
the user to select one if more than one is found.
-}
{-# LANGUAGE TupleSections #-}

module STLink.Detection
  ( pickBoard
  ) where

import           System.IO
import           System.USB

import           Data.Functor
import qualified Data.Vector            as V
import           Data.Word

import qualified Data.Text              as T
import           Text.PrettyPrint.Boxes

import           Numeric                (showHex)

-- | All STLINK boards have this vendor ID.
stVendorId :: Word16
stVendorId = 0x0483

-- | Useful to avoid having to get the device handle again, after
-- querying its description.
getDeviceAndDesc :: Device -> IO (Device, DeviceDesc)
getDeviceAndDesc d = (d, ) <$> getDeviceDesc d

-- | Utility function that turn an number into a hex string.
toHex :: (Show n, Integral n) => n -> String
toHex x = showHex x ""

-- | While we can easily request the *indices* for the description of
-- a USB device from the operating system, we actually need to query
-- the device itself to get the string that corresponds to the index.
getStrUSB :: (DeviceDesc -> Maybe StrIx) -> (Device, DeviceDesc) -> IO T.Text
getStrUSB f (d, dd) =
  withDeviceHandle d $ \h ->
    case f dd of
      Just i  -> getStrDescFirstLang h i 256
      Nothing -> pure ""

-- | Pretty-print a table of the available boards we detected.
printBoards :: [(Device, DeviceDesc)] -> IO ()
printBoards ds = do
  names <- traverse (getStrUSB deviceProductStrIx) ds
  serials <- traverse (getStrUSB deviceSerialNumberStrIx) ds
  let numbox =
        vcat right $
        "num" : (text . (\t -> "[" ++ t ++ "]") . show <$> [1 .. length ds])
      productbox =
        vcat right $ "id" : (text . toHex . deviceProductId . snd <$> ds)
      namebox = vcat left $ "name" : (text . T.unpack <$> names)
      serialbox = vcat left $ "serial" : (text . T.unpack <$> serials)
  printBox $ numbox <+> productbox <+> namebox <+> serialbox

-- | Find every ST board connected to the USB controller.
findStMicros :: IO (V.Vector (Device, DeviceDesc))
findStMicros
  -- set up a USB context and get all connected devices
 = do
  ctx <- newCtx
  devs <- getDevices ctx
  -- get a vector of device descriptors, filter out ST ones
  descs <- traverse getDeviceAndDesc devs
  pure $ V.filter ((== stVendorId) . deviceVendorId . snd) descs

-- | Ask the user to select one of the available board, or fail if
-- none are available.
pickBoard :: IO (Maybe Device)
pickBoard = do
  micros <- findStMicros
  printBoards (V.toList micros)
  case length micros of
    0 -> putStrLn "no boards detected!" $> Nothing
    1 -> putStrLn "auto-selecting board 1" $> Just (fst . V.head $ micros)
    _ -> do
      putStr "select board: "
      hFlush stdout
      num <- read <$> getLine
      pure $ fst <$> V.indexM micros (num - 1)
