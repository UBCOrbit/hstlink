{-|
Module      : STLink.Commands
Description : Commands for talking to an STLink dongle
Copyright   : (c) UBC Orbit
Maintainer  : sam.schweigel@ubcorbit.com
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module STLink.Commands
  (
  ) where

import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits

bitsOn :: (Num a, Bits a) => Int -> a
bitsOn n = f n 0
  where
    f 0 m = shiftR m 1
    f n m = f (n - 1) (shift (m .|. 1) 1)

selectBits :: (Integral a, Bits a, Integral n) => (Int, Int) -> a -> n
selectBits (p, w) n = fromIntegral $ shiftR n p .&. bitsOn w

class InCommand c where
  type InResponse c :: *
  inCommandEncoding :: c -> Put
  inResponseSize :: Int
  inResponseEncoding :: Get (InResponse c)

data CommandVersion =
  CommandVersion
  deriving (Show, Eq)

data ResponseVersion = ResponseVersion
  { versionStlink :: Int
  , versionJtag   :: Int
  , versionSwim   :: Int
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
