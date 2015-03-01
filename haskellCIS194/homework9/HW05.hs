module HW05 where

import Ring
import Parser
import Data.Maybe
import Data.Char
import Data.List (group)
import Text.Read (readMaybe)


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


-- | Exercise 3
-- >>> parse "[[" :: Maybe (Mat2x2, String)
-- Nothing
-- >>> parse "[[1,2][3,4]]" :: Maybe (Mat2x2, String)
-- Just (Mat2x2 1 2 3 4,"")
--
data Mat2x2 = Mat2x2 Integer Integer Integer Integer
    deriving (Show)

instance Ring Mat2x2 where
    addId = Mat2x2 0 0 0 0
    addInv (Mat2x2 a b c d) = Mat2x2 (negate a) (negate b) (negate c) (negate d)
    mulId = Mat2x2 1 0 1 0

    add (Mat2x2 a b c d) (Mat2x2 w x y z) = Mat2x2 (a + w) (b + x) (c + y) (d + z)
    mul (Mat2x2 a b c d) (Mat2x2 w x y z) = Mat2x2 (a * w + b * y) (a * x + b * z) (c * w + d * x) (c * x + d * z)

instance Parsable Mat2x2 where
    parse str = lstToMatrix $ strToList str
        where
        strToList xs = mapMaybe readMaybeInt $ group xs

lstToMatrix :: [Integer] -> Maybe (Mat2x2, String)
lstToMatrix (a:b:c:d:_) = Just (Mat2x2 a b c d, "")
lstToMatrix _ = Nothing

readMaybeInt :: String -> Maybe Integer
readMaybeInt = readMaybe

-- | Eq instnace for Mat2x2
-- >>> Mat2x2 1 2 3 4 == Mat2x2 1 2 3 4
-- True
-- >>> Mat2x2 1 2 3 4 /= Mat2x2 0 2 3 4
-- True
instance Eq Mat2x2 where
    (==) (Mat2x2 a b c d) (Mat2x2 m n o p) = a == m && b == n && c == o && d == p
