module HW08 where

import Data.Maybe
import Text.Read


-- | Exercise 1: detect wether a string as a certain format and return
-- true
-- >>> stringFitsFormat "3aaa2aa"
-- True
-- >>> stringFitsFormat "100a"
-- False
stringFitsFormat :: String -> Bool
stringFitsFormat = isJust . go
    where go :: String -> Maybe String
          go x:xs =
            case (readMaybe x :: Int ) of
                Just y:xs -> stripPrefix (replicate x y) xs
                _ -> Nothing
