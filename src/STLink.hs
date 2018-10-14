{-|
Module      : STLink
Description : Module that re-exports all user-facing functions
Copyright   : (c) UBC Orbit
Maintainer  : sam.schweigel@ubcorbit.com

This is the module users should import when writing applications.

= Short Tutorial

Interacting with an STLink board is really easy!  All STLink-board
related actions run inside the 'STLink' monad.  For example, we can
read @r0@, and then write its value + 3 back into @r1@.
@0x1234@:

> myAction :: STLink ()
> myAction = do
>   enterMode DebugSWD -- must be in debug mode to peek/poke registers
>   num <- readReg 0
>   writeReg 1 (num + 3)

'STLink' actions are board-agnostic, so in order to run them you have
to use the 'withBoard' function, that will automatically connect and
disconnect to the board.  It takes any STLink action, as well as a USB
'Device', and runs that action on that device.

In order to make things simpler, a helper function 'withAutoBoard' is
provided, that will scan for attached STLink dongles, and
automatically run the given action if one is available.  If more than
one is connected, a menu will prompt the user to select the desired
one.  We can use this to run our @myAction@ 'STLink' action:

> main :: IO ()
> main = withAutoBoard myAction
-}
module STLink
  ( module STLink.Driver
  , module STLink.Commands
  ) where

import           STLink.Commands
import           STLink.Driver   (withAutoBoard, withBoard, STLink)
