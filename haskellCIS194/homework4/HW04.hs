module HW04 where

import BST
import Data.Char
import Data.Maybe

-- | Exercise 13: insert into a BST
-- >>> insertBST compare 1 Leaf
-- Node Leaf 1 Leaf
-- >>> insertBST compare 1 (Node Leaf 1 Leaf)
-- Node Leaf 1 Leaf
-- >>> insertBST compare 2 (Node Leaf 1 Leaf)
-- Node Leaf 1 (Node Leaf 2 Leaf)
-- >>> insertBST compare (-1) (Node Leaf 1 (Node Leaf 2 Leaf))
-- Node (Node Leaf (-1) Leaf) 1 (Node Leaf 2 Leaf)
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ n Leaf = Node Leaf n Leaf
insertBST cmp n (Node left x right) = case cmp n x of
    LT -> Node (insertBST cmp n left) x right
    EQ -> Node left x right
    GT -> Node left x (insertBST cmp n right)


safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:_)  = Just x

-- | Exercise 14: check if the list of strings only contain capitalized
-- words
-- >>> allCaps ["Hi", "There"]
-- True
-- >>> allCaps []
-- True
-- >>> allCaps ["", "Bla"]
-- False
-- >>> allCaps ["Hi", "there"]
-- False
allCaps :: [String] -> Bool
allCaps = all (maybe False isUpper . listToMaybe)
