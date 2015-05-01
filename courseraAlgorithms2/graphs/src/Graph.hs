-- Graph using a List: no O(1) lookup
--
-- Order seems to be important the way you insert neighbours in the
-- adjacency list, since it influences the way searches generate the
-- neighbours to visit. Ok - not. The matter is not the order, but
-- applications like: find me the shortest path.
--
-- Doctests become slowly unmaintainable: they're missing setup code.
--
-- Quicktest tests are missing.
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

-- | Vertices of the Graph
--
vertices :: Graph k a -> [Vertex]
vertices = HM.keys

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

