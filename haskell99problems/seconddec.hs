{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List

-- encodeModified
-- modified version of the run-length encoding
-- no duplicates are simply copied into the list
--
data ListItem a =  Single a | Multiple Int a deriving Show

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified xs = foldr (\b a -> if length b == 1
                then (Single (head b)):a
                else (Multiple (length b) (head b)):a)
            []
            (group xs)


-- run-length encoding of a list (problem 10)
encodeSimple :: Ord a => [a] -> [(Int, a)]
encodeSimple xs = zip (map length $ group xs) (map head $ group xs)

-- encode' - modified solution for problem 11
-- uses map instead of foldr
-- credit goes to the author of the 11th solution on the haskell wiki
encodeModified' :: Ord a => [a] -> [ListItem a]
encodeModified' = map encodeHelper . encodeSimple
    where encodeHelper (1, x) = Single x
          encodeHelper (n, x) = Multiple n x

main = $(quickCheckAll)
