module DFS where

import Graph
import Data.List
import qualified Data.HashMap.Strict as HM
-- $setup
-- >>> let udg =  mkGraph addUndirectedEdge [(0,5),(4,3),(0,1),(6,4),(5,4),(0,2),(0,6),(5,3),(7,8)] HM.empty
-- >>> let dg =  mkGraph addDirectedEdge [(0,5),(4,3),(0,1),(6,4),(5,4),(0,2),(0,6),(5,3)] HM.empty
-- >>> let dg2 =  mkGraph addDirectedEdge [(0,5),(0,1),(2,0),(2,3),(0,6),(6,4),(5,4),(3,5),(7,6),(8,7),(6,9),(9,10),(9,11),(11,12),(9,12)] HM.empty

-- | depth first search
-- Find a path from a given to a goal Vertex
--
-- Test graph:
--
--       /------\
--      0 ---2   6
--      |\      /
--      | 1     |
--      |      /
--      5--3--4
--       \---/
--
-- >>> dfs 0 udg
-- [6,4,5,3,2,1]
--
-- Test graph as a directed graph:
--
--      /-------۷
--     0-->2    6
--     |\->1    |
--     |        |
--     ۷        ۷
--     5--->3<--4
--      \-------^
--
-- >>> dfs 0 dg
-- [6,4,3,2,1,5]
-- >>> dfs 2 dg
-- []
-- >>> dfs 5 dg
-- [3,4]
-- >>> dfs 1 dg
-- []
--
dfs :: Vertex -> Graph -> [Vertex]
dfs x = go [x] []
    where
        go [] [] _ = []
        go [] vs _ = tail vs
        go (n:xs) vs g
            | n `notElem` vs = go (adjToList n g ++ xs) (vs ++ [n]) g
            | otherwise = go xs vs g

-- | connected components
-- This code uses a single hashmap to create the list of connected
-- components. Instead of using a list to track visited items, we simply
-- use the presence or absence of a node in our hashmap to determine it
-- has been visited or not.
--
-- >>> let g = mkGraph addUndirectedEdge [(0,1),(7,8),(3,4),(4,5)] HM.empty
-- >>> cc 0 (HM.keys g) g HM.empty
-- fromList [(0,0),(1,0),(3,1),(4,1),(5,1),(7,2),(8,2)]
cc :: Int -> [Vertex] -> Graph -> HM.HashMap Vertex Int -> HM.HashMap Vertex Int
cc _ [] _ m = m
cc i (x:xs) g m
    | Nothing <- HM.lookup x m = cc (i+1) xs g (oneComponent i [x] g m)
    | otherwise = cc i xs g m

-- | helper to find one connected component
-- >>> let g = mkGraph addUndirectedEdge [(0,1),(7,8)] HM.empty
-- >>> oneComponent 0 [0] g HM.empty
-- fromList [(0,0),(1,0)]
--
oneComponent :: Int -> [Vertex] -> Graph -> HM.HashMap Vertex Int -> HM.HashMap Vertex Int
oneComponent _ [] _ m = m
oneComponent i (x:xs) g m
    | Nothing <- HM.lookup x m = oneComponent i xs' g (HM.insert x i m)
    | otherwise = oneComponent i xs g m
        where xs' = xs ++ dfs x g


-- | connected components, alternative implementation
-- This uses two folds to return sublists of connected components which
-- are not enumerated. The enumeration is provided by the amount of
-- sublists returned. It uses the unsafe `head` functions so expect it
-- to blow up.
--
-- >>> cc' $ mkGraph addUndirectedEdge [(0,1),(7,8)] HM.empty
--[[1,0],[8,7]]
cc' :: Graph -> [[Vertex]]
cc' g = foldl (\a x -> if sort (head a) == sort x then a else x:a) [head groups] groups
    where groups = foldl (\a x -> dfs x g : a) [] (HM.keys g)


-- | post order topological sort
--
-- The graph:
--
--   0 ------> 6 <--- 7 <--- 8
--  / \ <      |\
-- |   | |     | \
-- |   ۷ |     |  ۷
-- |   1 2     |  9---> 10
-- |     |     |  |\
-- | 3 </     /   ۷ -----۷
-- | ۷       |    11---> 12
-- ->5--->4<--
--
-- >>> let r = topologicalSort (HM.keys dg2) [] [] dg2
--
-- Reverse post order (without a stack, f y v m):
-- >>> reverse r
-- [8,7,2,3,0,5,1,6,4,9,10,11,12]
-- >>> topologicalSort (HM.keys dg) [] [] dg
-- [3,4,6,2,1,5,0]
--
-- Graph with a cycle. The current code does not detect the cycle, but
-- rather cuts out the edge we've just visited resulting in a
-- topological sort. :/
--
-- >>> let g = mkGraph addDirectedEdge [(0,1),(1,2),(1,3),(3,4),(4,5),(5,3)] HM.empty
-- >>> topologicalSort (HM.keys g) [] [] g
-- [5,3,4,2,1,0]
--
topologicalSort :: [Vertex] -> [Vertex] -> [Vertex] -> Graph -> [Vertex]
topologicalSort [] [] result _       = result
topologicalSort [] (x:mark) result g = topologicalSort mark [] (result ++ [x]) g
topologicalSort (x:xs) mark result g
    | x `notElem` result,
      x `notElem` mark   = topologicalSort xs [] result' g
    | otherwise          = topologicalSort xs mark result g
           where result' = topologicalSort (dfs x g) (x:mark) result g
