module Xasted.Asteroid (moveAsteroid, displayAsteroid) where

import Clash.Prelude
import Data.Bits (xor)

import Xasted.Types
import Xasted.Symbols



-- | input (move, reset, initH, initP), state (hPos, vPos) output (hPos, vPos, hitShip)

moveAsteroid :: (AsteroidWidth, Bit) -> (Bool, Bool, Bool, AsteroidWidth, Bit) -> ((AsteroidWidth, Bit), (AsteroidWidth, Bit, Bool))
moveAsteroid (h, v) (move, reset, flipV, ih, iv)
  | reset = ((ih, iv), (ih, iv, False))
  | h == 0 && move = ((0, v), (0, v, False))
  | move = ((h-1, v `xor` boolToBit flipV), (h-1, v `xor` boolToBit flipV , h == 1))
  | otherwise = ((h, v), (h, v, False))


-- | display on 7-segment for position p
displayAsteroid :: AsteroidWidth -> (AsteroidWidth, Bit) -> Bool -> Seg7
-- asteroid not displayed in 0 position
displayAsteroid 0 _ _ = blankSym
displayAsteroid p (h, v) hide = if not hide && p == h then asteroidSym v else blankSym
