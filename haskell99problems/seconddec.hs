{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List

-- encodeModified
-- modified version of the run-length encoding
-- no duplicates are simply copied into the list
--
data Encoded a =  Single a | Multiple Int a deriving Show

encode :: Eq a => [a] -> [Encoded a]
encode [x] = [Single x]
encode xs = foldl (\a b -> if length b == 1 then (Single (head b)):a else (Multiple (length b) (head b)):a) [] (group xs)
-- TODO: wrong order
-- yuck!

main = $(quickCheckAll)
