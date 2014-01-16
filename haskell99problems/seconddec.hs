{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List

-- encodeModified
-- modified version of the run-length encoding
-- no duplicates are simply copied into the list
--
data Encoded a =  Single a | Multiple Int a deriving Show

encode :: [a] -> [Encoded a]
encode [x] = [Single x]

main = $(quickCheckAll)
