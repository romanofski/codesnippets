-- Graph using a List: no O(1) lookup
--
-- Order seems to be important the way you insert neighbours in the
-- adjacency list, since it influences the way searches generate the
-- neighbours to visit. Ok - not. The matter is not the order, but
-- applications like: find me the shortest path.
--
module Graph where

import Data.Tuple (swap)
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L


type Edge = (Vertex, Vertex)

type Vertex = Int

type Graph k a = HM.HashMap Vertex    -- Vertex
                            [Vertex]  -- Neighbours XXX not a set, so keeping it free of duplicates is a performance hit


-- | Creates a graph from a list
-- >>> mkGraph addUndirectedEdge [(1,2)] HM.empty
-- fromList [(1,[2]),(2,[1])]
-- >>> mkGraph addDirectedEdge [(1,2)] HM.empty
-- fromList [(1,[2])]
--
mkGraph :: (Edge -> Graph k a -> Graph k a)     -- insertion function
           -> [Edge]                            -- list of edges to insert
           -> Graph k a                         -- graph to insert edges into
           -> Graph k a                         -- result
mkGraph _ ([]) g = g
mkGraph f (x:xs) g = mkGraph f xs (f x g)


-- | Addes an Edge to a Graph creating a directed or symmetric directed
-- graph.
--
addDirectedEdge :: Edge -> Graph k a -> Graph k a
addDirectedEdge (x,y) = HM.insertWith L.union x [y]


-- | Adds an Edge to the Graph.
--
-- >>> mkGraph addUndirectedEdge [(0,1),(0,2)] HM.empty
-- fromList [(0,[2,1]),(1,[0]),(2,[0])]
--
-- No duplicates are expected if we add the same value twice:
--
-- prop> addUndirectedEdge a (addUndirectedEdge a HM.empty) == addUndirectedEdge a HM.empty
addUndirectedEdge :: Edge -> Graph k a -> Graph k a
addUndirectedEdge e = addDirectedEdge e . addDirectedEdge (swap e)


-- | vertices adjacent to v
adj :: Vertex -> Graph k a -> Maybe [Vertex]
adj = HM.lookup

-- | safe helper to convert Maybe adj to list. [] is returned in case no
-- neighbours exist.
--
adjToList :: Vertex -> Graph k a -> [Vertex]
adjToList k g = concat $ maybeToList $ adj k g

-- | returns the amount of vertices
-- >>> let g = mkGraph addUndirectedEdge [(1,2), (1,2), (2, 3), (3, 3), (2, 5)] HM.empty
-- >>> numV g
-- 4
--
numV :: Graph k a -> Int
numV = HM.foldl' (\a _ -> a + 1) 0


-- | returns the amount of edges
-- >>> let g = mkGraph addUndirectedEdge [(1,2),(2,3),(2,2)] HM.empty
-- >>> numE g == length (HM.toList g)
-- True
--
numE :: Graph k a -> Int
numE = HM.size


-- | degree of a vertice
--
degree :: Vertex -> Graph k a -> Int
degree x g =
    case adj x g of
        Just vs -> foldl (\a _ -> a + 1) 0 vs
        Nothing -> 0

-- | max degree
--
maxDegree :: Graph k a -> Int
maxDegree g = maximum $ fmap length vals
    where vals = mapMaybe (`adj` g) (HM.keys g)

-- | average degree
--
avgDegree :: Graph k a -> Double
avgDegree g = 2.0 * (e / v)
    where e = fromIntegral $ numE g
          v = fromIntegral $ numV g


-- | count self loops
-- >>> countSelfLoops $ mkGraph addUndirectedEdge [(1,2),(2,3),(2,2)] HM.empty
-- 1
-- >>> countSelfLoops $ mkGraph addUndirectedEdge [(1,2),(1,1),(1,4),(2,2)] HM.empty
-- 2
--
countSelfLoops :: Graph k a -> Int
countSelfLoops = HM.foldlWithKey' (\a k v -> if k `elem` v then a + 1 else a) 0


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
-- >>> let g =  mkGraph addUndirectedEdge [(0,5),(4,3),(0,1),(6,4),(5,4),(0,2),(0,6),(5,3)] HM.empty
-- >>> dfs 0 g
-- [0,6,2,1,5,4,3]
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
-- >>> let g =  mkGraph addDirectedEdge [(0,5),(4,3),(0,1),(6,4),(5,4),(0,2),(0,6),(5,3)] HM.empty
-- >>> dfs 2 g
-- [0,6,2,1,5,4,3]
--
dfs :: Vertex -> Graph k a -> [Vertex]
dfs x g = search getChildren (head children) (tail children) [x] g
    where children = getChildren x g []

getChildren :: Vertex -> Graph k a -> [Vertex] -> [Vertex]
getChildren k g xs = xs ++ adjToList k g


-- | breadth first search
-- Find a path from a given to a goal Vertex
--
-- We re-use the same test graph as for the dfs implementation.
-- >>> let g =  mkGraph addUndirectedEdge [(0,5),(4,3),(0,1),(6,4),(5,4),(0,2),(0,6),(5,3)] HM.empty
-- >>> bfs 0 g
-- [0,6,2,4,1,5,3]
--
bfs :: Vertex -> Graph k a -> [Vertex]
bfs x g =
    let f = getBFSChildren
    in search f (head $ f x g []) (tail $ f x g []) [x] g

getBFSChildren :: Vertex -> Graph k a -> [Vertex] -> [Vertex]
getBFSChildren k g xs = adjToList k g ++ xs


-- | generic search algorithm
--
search :: (Vertex -> Graph k a -> [Vertex] -> [Vertex])
        -> Vertex                        -- start node
        -> [Vertex]                      -- nodes to_visit
        -> [Vertex]                      -- nodes visited
        -> Graph k a                     -- the graph
        -> [Vertex]                      -- returned visited
search _ _ [] [] _ = []
search _ _ [] vs _ = vs
search f x (y:xs) vs g
    | x `notElem` vs = search f y (f x g xs) (vs ++ [x]) g
    | otherwise = search f y xs vs g
