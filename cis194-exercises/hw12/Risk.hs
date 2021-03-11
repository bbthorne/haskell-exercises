{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

-- battle simulates one battle in Risk where each side uses the max number of
-- units possible.
battle :: Battlefield -> Rand StdGen Battlefield
battle armies = (rolls attackForce) >>= \a ->
                (rolls defendForce) >>= \d ->
                return $ foldr damage armies (zip a d)
  where attackForce   = min 3 $ attackers armies - 1
        defendForce   = min 2 $ defenders armies
        rolls n       = sortBy (flip compare) <$> replicateM n die
        damage (a,d) b
          | a > d     = Battlefield (attackers b) (defenders b - 1)
          | otherwise = Battlefield (attackers b - 1) (defenders b)

-- invade simulates an invasion attempt and runs battle until there are no
-- defenders or fewer than 2 attackers remaining.
invade :: Battlefield -> Rand StdGen Battlefield
invade armies
  | defenders armies == 0 || attackers armies < 2 = return armies
  | otherwise = battle armies >>= invade

-- successProb calculates the probability that the attacker will completely
-- destroy the defender based on 1000 invasion simulations
successProb :: Battlefield -> Rand StdGen Double
successProb armies = invasions >>= succProb
  where numBattles = 1000
        invasions  = replicateM numBattles $ invade armies
        succProb   = calcProb . length . filter ((==) 0 . defenders)
        calcProb n = return $ fromIntegral n / fromIntegral numBattles
