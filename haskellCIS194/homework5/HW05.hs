module HW05 where

import Ring
import Parser
import Data.Maybe


-- | Exercise 1
-- >>> intParsingWorks
-- True
intParsingWorks :: Bool
intParsingWorks = (parse "3" == Just (3 :: Integer, "")) &&
                  (parseRing "1 + 2 * 5" == Just (11 :: Integer)) &&
                  (addId == (0 :: Integer))


data Mod5 = MkMod Integer
    deriving (Show, Eq)

instance Ring Mod5 where
    addId = MkMod 0
    addInv (MkMod x) = MkMod (((-x `mod` 5) + (x `mod` 5)) `mod` 5)
    mulId = MkMod 1

    add (MkMod x) (MkMod y) = MkMod ((x + y) `mod` 5)
    mul (MkMod x) (MkMod y) = MkMod ((x * y) `mod` 5)


instance Parsable Mod5 where
    parse str = fmap (\(n,s) -> (MkMod n,s)) (listToMaybe $ reads str)


-- | Exercise 2
-- >>> mod5ParsingWorks
-- True
mod5ParsingWorks :: Bool
mod5ParsingWorks = (parse "10" == Just (MkMod 10, "")) &&
                   (parseRing "1 + 2 * 3" == Just (MkMod 2))
