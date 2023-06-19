module Xasted.Symbols where

import Clash.Prelude

import Xasted.Types


blankSym :: Seg7
blankSym = 0b11111111

shipSym :: Seg7
shipSym = 0b11000110

-- | map to high or low bullet
bulletSym :: Bit -> Seg7
bulletSym 0 = 0b11111110
bulletSym _ = 0b11110111

-- | map to high or low asteroid
asteroidSym :: Bit -> Seg7
asteroidSym 0 = 0b10011100
asteroidSym _ = 0b10100011


-- XASTED letters
symX :: Seg7
symX = 0b10110110 -- greek Xi
symA :: Seg7
symA = 0b10001000
symS :: Seg7
symS = 0b10010010
symT :: Seg7
symT = 0b10000111
symE :: Seg7
symE = 0b10000110
symD :: Seg7
symD = 0b10100001
