module Xasted.Bullet (moveBullet, displayBullet) where

import Clash.Prelude

import Xasted.Types
import Xasted.Symbols


-- | input (tick, remove, key0, key1), state (hPos, vPos), output (hPos, vPos)
moveBullet :: (ScreenWidth, Bit) -> (Bool, Bool, Bool, Bool) -> ((ScreenWidth, Bit), (ScreenWidth, Bit))
moveBullet (h, v) (move, remove, fireTop, fireBottom)
  | remove = ((0, v), (0, v))
  | move && h == 0 = ((0, v), (0, v))
  | move && h == maxBound = ((0, v), (0, v))
  | move = ((h + 1, v), (h + 1, v))
  | fireTop && h==0 = ((1, 0), (1, 0))
  | fireBottom && h==0 = ((1, 1), (1, 1))
  | otherwise = ((h, v), (h, v))

-- | display the bullet for position p
displayBullet :: ScreenWidth -> (ScreenWidth, Bit) -> Seg7
-- bullet not displayed in 0 position
displayBullet 0 _ = blankSym
displayBullet p (h, v) = if p == h then bulletSym v else blankSym
