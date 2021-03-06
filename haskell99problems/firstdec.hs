{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List

-- returns the last element from mapping
myLast :: [a] -> a
myLast [] = error "empty list"
myLast xs = head $ reverse xs

prop_myLast xs = length xs > 0 ==> myLast xs == last xs

myLast2 :: Ord a => [a] -> a
myLast2 (x:[]) = x
myLast2 (_:xs) = myLast2 xs

prop_myLast2 xs = length xs > 0 ==> myLast2 xs == last xs

-- returns the second last element
myButLast :: Ord a => [a] -> a
myButLast [] = error "empty list"
myButLast xs = reverse xs !! 1

prop_myButLast xs = length xs > 2 ==> myButLast xs == last (init xs)

-- returns element from mapping by given index
elementAt :: Ord a => [a] -> Int -> a
elementAt xs y = xs !! (y - 1)

-- prints length of a mapping
myLength :: Ord a => [a] -> Int
myLength [] = 0
myLength xs = sum [1 | x <- xs ]

prop_myLength xs = myLength xs == length xs

-- reverses a list
reverse' :: Ord a => [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

prop_reverse xs = reverse' xs == reverse xs

-- checks if the input is a palindrome
-- it consumes the mapping from left and right comparing each. The
-- special case is the middle letter, which we also result to True
-- (e.g. otto == ottto)
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs)
    | x == t = isPalindrome $ init xs
    | otherwise = False
        where t = last xs

isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' xs = foldr (\(x,y) acc -> if x == y then acc else False) True (zip xs $ reverse xs)


-- flatten
-- flattens a nested list. We use a new type since the wiki page advises
-- us: lists in Haskell are homogenous
data NestedList a = Elem a | List [NestedList a] deriving Show
flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

flattenf :: NestedList a -> [a]
flattenf (Elem x) = [x]
flattenf (List xs) = foldr (++) [] (map flattenf xs)


-- compress
-- eliminates consecutive duplicates of list elements without using
-- group
compress :: Ord a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs)
    | x == (head xs) = compress xs
    | otherwise = [x] ++ compress xs


-- pack
-- packs duplicates into sub lists
pack :: Ord a => [a] -> [[a]]
pack [] = []
pack xs = [sub] ++ pack rest
    where x = head xs
          sub = takeWhile (== x) xs
          rest = dropWhile (== x) xs

prop_pack xs = pack xs == group xs


-- encode
-- run-length encoding of a list
encode :: Ord a => [a] -> [(Int, a)]
encode xs = zip (map length $ group xs) (map head $ group xs)

main = $(quickCheckAll)
