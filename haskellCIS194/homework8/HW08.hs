module HW08 where

import Data.Maybe
import Text.Read
import Data.List
import Data.Monoid
import Control.Monad.Random
import Control.Monad (replicateM)

-- | Exercise 1: detect wether a string as a certain format and return
-- true
-- >>> stringFitsFormat "3aaa2aa"
-- True
-- >>> stringFitsFormat "100a"
-- False
-- >>> stringFitsFormat "001a"
-- True
-- >>> stringFitsFormat "2bb2bb"
-- False
-- >>> stringFitsFormat "0"
-- True
-- >>> stringFitsFormat "1"
-- False
stringFitsFormat :: String -> Bool
stringFitsFormat = isJust . go


-- |
-- >>> go "3aaa"
-- Just ""
-- >>> go "100"
-- Nothing
-- >>> go "0"
-- Just ""
go :: String -> Maybe String
go "0" = Just ""
go [] = Just ""
go [_] = Nothing
go (x:xs) = do
    n <- readMaybe [x] :: Maybe Int
    rest <- stripPrefix (replicate n 'a') xs
    go rest

-- | Exercise 2
-- list of all numbers between 1 and 100 that are divisible by 5 and 7
-- Note: the 35 and 70 is a bit of a cheat I agree
specialNumbers :: [Int]
specialNumbers = [ n | n <- [1..100], n `mod` 5 == 0 && n /= 35 && n /= 70]


-- | Risk
--
type StdRand = Rand StdGen
type Army = Int
data ArmyCounts = ArmyCounts { attackers :: Army, defenders :: Army }
    deriving Show

type DieRoll = Int


-- | Exercise 3
-- simulates rolling a fair, 6-sided die
dieRoll :: StdRand DieRoll
dieRoll = getRandomR (1,6)

-- | Exercise 4
-- Monoid instance for ArmyCounts to make our life easier.
-- >>> let a = ArmyCounts {attackers = 0, defenders = -1}
-- >>> let b = ArmyCounts {attackers = -1, defenders = 0}
-- >>> a `mappend` b
-- ArmyCounts {attackers = -1, defenders = -1}
--
instance Monoid ArmyCounts where
    mempty                                    = ArmyCounts { attackers = 0, defenders = 0 }
    mappend (ArmyCounts a b) (ArmyCounts x y) = ArmyCounts { attackers = a + x, defenders = b + y}


-- | Exercise 4
-- computes the change in the number of armies resulting from the rolls
-- >>> battleResults [3,6,4] [5,5]
-- ArmyCounts {attackers = -1, defenders = -1}
-- >>> battleResults [3,6,4] [5,6]
-- ArmyCounts {attackers = -2, defenders = 0}
-- >>> battleResults [4] [3,2]
-- ArmyCounts {attackers = 0, defenders = -1}
--
battleResults :: [DieRoll] -> [DieRoll] -> ArmyCounts
battleResults ab cd = roll sx sy
    where sx = sortBy (flip compare) ab
          sy = sortBy (flip compare) cd
          roll :: [DieRoll] -> [DieRoll] -> ArmyCounts
          roll (a:xs) (b:ys)
            | a > b = ArmyCounts { attackers = 0, defenders = -1} `mappend` roll xs ys
            | otherwise =  ArmyCounts { attackers = -1, defenders = 0 } `mappend` roll xs ys
          roll _ _ = ArmyCounts 0 0


-- | Exercise 5
-- simulates single battle
--
battle :: ArmyCounts -> StdRand ArmyCounts
battle (ArmyCounts a b) = do
    let turns = minimum [a,b]
    attackersRolls <- replicateM turns dieRoll
    defendersRolls <- replicateM turns dieRoll
    let r = battleResults attackersRolls defendersRolls
    return r
