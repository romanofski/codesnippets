module DFS where

import Graph
import qualified Data.HashMap.Strict as HM
-- $setup
-- >>> let udg =  mkGraph addUndirectedEdge [(0,5),(4,3),(0,1),(6,4),(5,4),(0,2),(0,6),(5,3)] HM.empty
-- >>> let dg =  mkGraph addDirectedEdge [(0,5),(4,3),(0,1),(6,4),(5,4),(0,2),(0,6),(5,3)] HM.empty

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
-- [0,6,4,5,3,2,1]
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
-- [0,6,4,3,2,1,5]
-- >>> dfs 2 dg
-- [2]
-- >>> dfs 5 dg
-- [5,3,4]
-- >>> dfs 1 dg
-- [1]
--
dfs :: Vertex -> Graph k a -> [Vertex]
dfs x = go [x] []
    where
        go [] vs _ = vs
        go (n:xs) vs g
            | n `notElem` vs = go (adjToList n g ++ xs) (vs ++ [n]) g
            | otherwise = go xs vs g

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
-- >>> let g =  mkGraph addDirectedEdge [(0,5),(0,1),(2,0),(2,3),(0,6),(6,4),(5,4),(3,5),(7,6),(8,7),(6,9),(9,10),(9,11),(11,12),(9,12)] HM.empty
-- >>> topologicalSort [0] [] g
-- [8,7,2,3,0,6,9,10,11,12,1,5,4]
topologicalSort :: [Vertex] -> [Vertex] -> Graph k a -> [Vertex]
topologicalSort [x] [] g = topologicalSort [] [x] g
topologicalSort [] vs _ = vs
topologicalSort (x:xs) vs g
    | x `notElem` vs = topologicalSort (dfs x g) vs g
    | otherwise = topologicalSort xs (x:vs) g
