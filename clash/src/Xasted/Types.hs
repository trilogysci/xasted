module Xasted.Types where

import Clash.Prelude

-- | Type for 7-segment display
type Seg7 = BitVector 8
-- | Type for on-screen motion (bullet)
type ScreenWidth = Index 6
-- | Type for asteroid motion
type AsteroidWidth = Index 32
-- | Type for Lives (total is N - 1)
type Lives = Index 4

data Level = Beginner | Intermediate | Advanced | Expert
  deriving (Eq, Show)