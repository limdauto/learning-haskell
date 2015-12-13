{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Applicative
import Control.Monad
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

instance Show Battlefield where
    show bf = show (attackers bf) ++ " " ++ show (defenders bf)

legalDef n = max 0 (min 2 n)
legalAtt n = max 0 (min 3 (n - 1))
rollDice n = replicateM n die

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
    let numAttackers = attackers bf
    let numDefenders = defenders bf
    let legalAttacks = legalAtt numAttackers
    let legalDefends = legalDef numDefenders
    a <- sortBy (flip compare) <$> rollDice legalAttacks
    d <- sortBy (flip compare) <$> rollDice legalDefends
    let attackWins = length . (filter (==True)) $ zipWith (>) a d
    return Battlefield { attackers = numAttackers - (legalAttacks - attackWins) :: Army
                       , defenders = numDefenders - attackWins :: Army }

invade :: Battlefield -> Rand StdGen Battlefield
invade bf
    | attackers bf < 2  = return bf
    | defenders bf == 0 = return bf
    | otherwise = do
        bf' <- battle bf
        invade bf'

sim :: Battlefield -> Integer -> Integer -> Rand StdGen Integer
sim bf n wins
    | n == 0 = return wins
    | otherwise = do
        bf' <- invade bf
        let wins' = if defenders bf' > 0 then wins else wins + 1
        sim bf (n - 1) wins'

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
    wins <- sim bf 1000 0
    return $ (fromIntegral wins) / 1000
