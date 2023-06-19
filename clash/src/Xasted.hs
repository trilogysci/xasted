{-# Language NumericUnderscores #-}
module Xasted (topEntity) where

import Clash.Prelude


import Clash.Signal
import Clash.Prelude
import Clash.Intel.ClockGen
import Clash.Class.Counter
import qualified Clash.Sized.Vector as V
import qualified Clash.Explicit.Prelude as ExPr

import Xasted.Types
import Xasted.Bullet
import Xasted.Asteroid
import Xasted.Symbols



-- PortName "SW"
{-# ANN topEntity
  (Synthesize
    { t_name   = "xasted"
    , t_inputs = [ PortName "MAX10_CLK1_50", PortName "KEY"]
    , t_output = PortProduct "" [
        PortName "HEX0",
        PortName "HEX1",
        PortName "HEX2",
        PortName "HEX3",
        PortName "HEX4",
        PortName "HEX5",
        PortName "LEDR" ]
    }) #-}

topEntity
  :: Clock System
  -> Signal System (BitVector 2)
  -> Signal System (Seg7, Seg7, Seg7, Seg7, Seg7, Seg7, BitVector 10)
topEntity clk keys = withClockResetEnable clk rstSync en (xasted press0 press1)
  where
    -- Don't need a reset or enable
    en = enableGen
    rstSync = unsafeToReset (pure False)
    -- extract and invert keys
    press0 = fmap (\bv -> not $ testBit bv 0) keys
    press1 = fmap (\bv -> not $ testBit bv 1) keys

-- | create a reset from an ith element of keys
resetFromKeys :: (KnownDomain dom, KnownNat n) => Int -> Signal dom (BitVector n) -> Reset dom
resetFromKeys ix bv = unsafeFromHighPolarity (fmap (\bv -> testBit bv ix) bv)

selectBit :: KnownNat n => Int ->  BitVector n -> Bit
selectBit ix bv = boolToBit $ testBit bv ix

type AsteroidCounter = Index 30000000
type BulletCounter =   Index 10000000
type FlashCounter =    Index  2000000
version = 0b01_0000_0000 :: BitVector 10

xasted :: 
  HiddenClockResetEnable dom =>
  Signal dom Bool
  -> Signal dom Bool
  -> Signal dom (Seg7, Seg7, Seg7, Seg7, Seg7, Seg7, BitVector 10)
xasted press0 press1 = bundle (hex0,hex1,hex2,hex3,hex4,hex5, leds)
  where
    speed = fmap speedCtrl (level)
    asteroidTick = mealy subClockVar (0 :: AsteroidCounter)  speed
    -- asteroidTick = mealy subClock (0 :: AsteroidCounter) (pure ())

    bulletTick = mealy subClock (0 :: BulletCounter) (pure ())
    flashTick = mealy subClock (0 :: FlashCounter) (pure ())
    newGame = isRising False playing
    level = mealy levelCtrl 0 $ bundle (shot, newGame)
    onScreenTick = fmap (\(tick, pos) -> tick && pos <= ( toEnum $ fromEnum $ (maxBound :: ScreenWidth)) ) $ bundle (asteroidTick, asteroidHPos)
    onScreenPos = fmap (toEnum . fromEnum) asteroidHPos
    flipV = mealyB flipCtrl 0 (level, onScreenPos, onScreenTick)
    -- tick = pure 0
    -- extract and invert keys
    -- flipV = pure True
    -- TODO delay while flashing
    fireTop = isRising False press0
    fireBottom = isRising False press1
    rand = mealyB randomCollector (0, 0) (fireTop, fireBottom)
    playingP = delay False playing
    hitShipP = delay False hitShip
    bulletMove = bulletTick .&&. playingP
    canMove = playingP .&&. fmap not dontMove
    asteroidMove = asteroidTick .&&. canMove
    startPlay = isRising False playingP
    resetAsteroid =  startPlay .||. hitShipP .||. resetAsteroid0P
    randV = fmap (lsb) rand
    randH = fmap (\r -> (bv2i $ resize $ slice d3 d1 r) + (5 ::AsteroidWidth)) rand -- range [5, 12] (alternatively [5, 21))
    (bulletHPos, bulletVPos) = mealyB moveBullet (0, 0) (bulletMove, shotP, fireTop, fireBottom)
    (asteroidHPos, asteroidVPos, hitShip) = mealyB moveAsteroid (9, 0) (asteroidMove, resetAsteroid, flipV, randH, randV)
    -- use previous bullet position to avoid glitch where both moving at the same time
    bulletHPosP = delay 0 bulletHPos
    positions = bundle (bulletHPosP, bulletVPos, asteroidHPos, asteroidVPos)
    same = fmap (\(bh,bv,ah,av) -> bh > 0 && ah == (zeroExtend bh) && av == bv) positions
    shot = isRising False same
    shotP = delay False shot
    (flashCt) = mealyB shotCtrl (8) (flashTick, shot)
    dontMove = fmap (> 0) flashCt
    resetAsteroid0 = fmap (== 1) flashCt
    resetAsteroid0P = delay False resetAsteroid0
    hideAsteroid = fmap (bitToBool . lsb) flashCt
    positionsH = bundle (bulletHPos, bulletVPos, asteroidHPos, asteroidVPos, hideAsteroid)
    (shipHide) = mealyB flashCounter 0 (flashTick, hitShip)
    shipFlashSym = mux shipHide (pure blankSym) (pure shipSym)
    resetGame = isRising False press0 .||. isRising False press1
    -- hitShip = isRising False press1
    -- hitShip = pure False
    (playing, lives) = mealyB gameCtrl (False, 0) (hitShip, resetGame)
    -- TODO add ship and asteroid flashing
    hex5 = displayGameOr playing symX shipFlashSym
    hex4 = displayGameOr playing symA $ bulletOrAsteroid 1 positionsH
    hex3 = displayGameOr playing symS $ bulletOrAsteroid 2 positionsH
    hex2 = displayGameOr playing symT $ bulletOrAsteroid 3 positionsH
    hex1 = displayGameOr playing symE $ bulletOrAsteroid 4 positionsH
    hex0 = displayGameOr playing symD $ bulletOrAsteroid 5 positionsH
    ticks = mealy toggleTick False asteroidTick
    gameD = fmap gameDisplay (bundle (lives, level))
    leds = mux (playing) gameD (pure version)
    -- Various debugging options
    -- leds = mealy tickCounter 0 asteroidTick
    -- leds = fmap debugLeds (bundle (same, shotTog, dontMove, hideAsteroid, resetAsteroid0, ticks))
    -- leds = fmap (resize) rand
    -- leds = fmap (toEnum . fromEnum) randH


gameDisplay (lives, level) = displayLives lives .|. displayLevel level

displayLevel :: Level -> BitVector 10
displayLevel Beginner = 0b0001_0000
displayLevel Intermediate = 0b0010_0000
displayLevel Advanced = 0b0011_0000
displayLevel Expert = 0b0100_0000

bulletOrAsteroid ::
  ScreenWidth
  -> Signal dom (ScreenWidth, Bit, AsteroidWidth, Bit, Bool)
  -> Signal dom (Seg7)
bulletOrAsteroid pos = fmap (\(bh,bv,ah,av,ahide) -> displayBullet pos (bh,bv) .&. displayAsteroid (resize pos) (ah,av) ahide)

-- | Display game title symbol or game symbol depending on playing
displayGameOr :: Signal dom Bool -> Seg7 -> Signal dom Seg7 -> Signal dom Seg7
displayGameOr playing titleSym gameSym = mux playing gameSym (pure titleSym)

-- | Check if asteroid was shot and output a shot, dontMove, resetAsteroid, hideAsteroid
shotAsteroid ::
  (ScreenWidth, Bit, AsteroidWidth, Bit)
  -> Bool
shotAsteroid (bh,bv,ah,av) = shot
    where
      shot = bh > 0 && av == bv && ah == (zeroExtend bh)
      {--
      resetAsteroid = flashCount == 1
      flashCount' = if shot then 8 else (if flashCount == 0 && not tick then flashCount else flashCount - 1)
      dontMove = flashCount' > 0
      hideAsteroid = testBit flashCount' 0 -}

toggleCtrl :: Bool -> Bool -> (Bool, Bool)
toggleCtrl val trig = (val', val')
  where
    val' = if trig then not val else val

shotCtrl ::
  Index 30
  -> (Bool, Bool)
  -> (Index 30, (Index 30))
shotCtrl (flashCount) (tick, shot) = (flashCount', (flashCount'))
    where
      flashCount' = if shot then maxBound else (if not tick || flashCount == 0 then flashCount else flashCount - 1)

-- | display lives counter (and debugging leds)
displayLives :: (Lives) ->  BitVector 10
displayLives (lives) = (zeroExtend $ pack (lives > 2, lives > 1, lives > 0))

-- | Display debugging
debugLeds :: (Bool, Bool, Bool, Bool, Bool, Bool) -> BitVector 10
debugLeds (key0, key1, tick, dontMove, hitShip, x) = pack (key0, key1, tick, dontMove, hitShip, x) ++# (0 :: BitVector 4)

-- | Game Contol input (hitShip, reset), state (playing, lives), output (playing, lives)
gameCtrl :: (Bool, Lives) -> (Bool, Bool) -> ((Bool, Lives), (Bool, Lives))
gameCtrl (playing, lives) (shipHit, restart)
  | restart && not playing = ((True, maxBound), (True, maxBound))
  | not playing || lives == 0 = ((False, 0), (False, 0))
  | shipHit = ((True, lives - 1), (True, lives - 1))
  | otherwise = ((playing, lives), (playing, lives))

-- | tick on a sub clock specified by the index size
subClock :: KnownNat n => Index n -> () -> (Index n, Bool)
subClock 0 _ = (maxBound, True)
subClock b _ = (b - 1, False)

-- | variable sub clock tick based on the input
subClockVar :: KnownNat n => Index n -> Index n -> (Index n, Bool)
subClockVar count maxVal
  | count >= maxVal = (0, True)
  | otherwise = (count + 1, False)


subClockCtr :: KnownNat n => Index n -> () -> (Index n, (Bool, Index n))
subClockCtr 0 _ = (maxBound, (True, maxBound))
subClockCtr b _ = (b - 1, (False, b - 1))

clockCounter :: KnownNat n => BitVector n -> () -> (BitVector n, BitVector n)
clockCounter b _ = (b + 1, b + 1)

toggleTick :: Bool -> Bool -> (Bool, Bool)
toggleTick ticks tick = (ticks', ticks')
  where
    ticks' = if tick then not ticks else ticks

tickCounter :: BitVector 10 -> Bool -> (BitVector 10, BitVector 10)
tickCounter ticks tick = (ticks', ticks')
  where
    ticks' = if tick then ticks + 1 else ticks

-- | when start is true, flash until
flashCounter :: Index 8 -> (Bool, Bool) -> (Index 8, Bool)
flashCounter counter (tick, start)
  | start = (maxBound, hide)
  | tick && counter > 0 = (counter - 1, hide)
  | otherwise = (counter, hide)
  where
    hide = testBit counter 0

-- | collect random values from button press timing
randomCollector :: (BitVector 16,BitVector 16) -> (Bool, Bool) -> ((BitVector 16,BitVector 16), BitVector 16)
randomCollector (counter, randomValue) (press1, press2) = ((counter', randomValue'), randomValue')
  where
    counter' = counter + 1
    randomValue' = if press1 || press2 then randomValue `xor` counter else randomValue


levelCtrl :: (Index 16) -> (Bool, Bool) -> (Index 16, Level)
levelCtrl (shotCount) (shot, reset)
  | reset = (0, Beginner)
  | shot && shotCount < 15 = (shotCount + 1, level (shotCount + 1))
  | otherwise = (shotCount, level shotCount)
  where
    level cnt
     | cnt < 5 = Beginner
     | cnt < 10 = Intermediate
     | cnt < 15 = Advanced
     | otherwise = Expert

speedCtrl :: Level -> AsteroidCounter
speedCtrl Beginner = maxBound
speedCtrl Intermediate = maxBound `div` 2
speedCtrl Advanced = maxBound `div` 4
speedCtrl Expert = maxBound `div` 8

-- | depending on the level cycles between several patterns
flipModeM :: Vec 4 (BitVector 6)
flipModeM =
    0b101010 :>
    0b100100 :>
    0b001001 :>
    0b111111 :> Nil

-- | determine when to flip the asteroid between top and bottom based on the level
flipCtrl :: (Index 4) -> (Level, ScreenWidth, Bool) -> (Index 4, Bool)
flipCtrl (counter) (level, pos, tick)
  | level == Intermediate && tick = (counter + 1, bit flipModeM)
  | level == Advanced && tick && counter == 2 = (0, bit flipModeM)
  | level == Advanced && tick = (counter + 1, bit flipModeM)
  | level == Expert && tick = (counter + 1, bit flipModeM)
  | otherwise = (counter, False)
  where
    bit flipMode = flipMode !! 0 `testBit` fromEnum pos
