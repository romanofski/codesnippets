module HW08 where

import Data.Maybe
import Text.Read
import Data.List


-- | Exercise 1: detect wether a string as a certain format and return
-- true
-- >>> stringFitsFormat "3aaa2aa"
-- True
-- >>> stringFitsFormat "100a"
-- False
stringFitsFormat :: String -> Bool
stringFitsFormat = isJust . go


-- |
-- >>> go "3aaa"
-- Just ""
-- >>> go "100"
-- Nothing
go :: String -> Maybe String
go [] = Just ""
go (x:[]) = Nothing
go (x:y:xs) = do
    n <- readMaybe [x] :: Maybe Int
    rest <- stripPrefix (replicate n y) (y : xs)
    go rest
