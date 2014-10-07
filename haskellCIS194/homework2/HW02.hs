module HW02 where

import Words
import Data.List

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- | Exercise 1
-- >>> formableBy "fun" ['x','n','i','f','u','e','l']
-- True
-- >>> formableBy "haskell" ['k','l','e','h','a','l','s']
-- True
-- >>> formableBy "haskell" ['k','l','e','h','a','y','s']
-- False
-- >>> formableBy "" ['a','f']
-- True
formableBy :: String -> Hand -> Bool
formableBy xs ys = null (xs \\ ys)

-- | Exercise 2
-- >>> wordsFrom ['a','b','c','d']
--["ab","ad","ba","bad","cab","cad","dab"]
wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords


-- | Exercise 3
-- See if we can form the word given by string and template with the
-- given letters.
-- >>> wordFitsTemplate "let" ['x','x'] "let"
-- True
-- >>> wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "care"
-- True
-- >>> wordFitsTemplate "??r?" ['c','x','e','w','b','c','l'] "care"
-- False
wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate t h s = formableBy ns h
    where ns = s \\ t


-- | Exercise 4
-- Calculate scrabble value by word
-- scrabbleValueWord "care"
-- 6
-- scrabbleValueWord "quiz"
-- 22
scrabbleValueWord :: String -> Int
scrabbleValueWord xs = sum $ map scrabbleValue xs


-- | Returns every string which tuple value is equal to given number
-- >>> filterByValue [(3,"1"), (5,"2")] 3
-- ["1"]
filterByValue :: [(Int, String)] -> Int -> [String]
filterByValue ((x,s):xs) y
    | x == y = s : filterByValue xs y
    | otherwise = []

-- | Exercise 5
-- filter words with max point value
-- bestWords ["cat", "rat", "bat"]
-- ["bat","cat"]
-- bestWords []
-- []
bestWords :: [String] -> [String]
bestWords xs = filterByValue new m
    where val = map scrabbleValueWord xs
          new = zip val xs
          m = maximum val
