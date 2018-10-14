# HSTLink

This is a library for interacting with STLink dongles and debugging
STM32 microcontrollers through haskell scripts!

At UBC Orbit, we're using it to simulate upsets that the satellite
with encounter in space, and ensuring that they're dealt with
accordingly.

## Example

```haskell
import STLink

fiddleRegs :: STLink ()
fiddleRegs = withAutoBoard $ do
  enterMode DebugSWD -- must be in debug mode to peek/poke registers
  num <- readReg 0
  writeReg 1 (num + 3)
```
