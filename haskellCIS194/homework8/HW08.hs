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
