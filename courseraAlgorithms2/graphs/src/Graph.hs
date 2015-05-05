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


-- | Edge connecting two vertices
--
data Edge = Edge (Vertex, Vertex) Weight
    deriving (Show, Eq)

type Vertex = Int
type Weight = Double

type Graph k a = HM.HashMap Vertex  -- Vertex
                            [Edge]  -- Neighbours XXX not a set, so keeping it free of duplicates is a performance hit


-- | creates Edge from a 3-Tuple
--
-- >>> edgeFromTuple (1,2,3)
-- Edge (1,2) 3.0
--
edgeFromTuple :: (Vertex, Vertex, Weight) -> Edge
edgeFromTuple (x, y, w) = Edge (x, y) w

-- | builds a directed Graph from a list of 3-Tuples
--
buildDirectedGraph :: [(Vertex, Vertex, Weight)] -> Graph k a
buildDirectedGraph = foldl (\a t -> mkGraph addDirectedEdge [edgeFromTuple t] a) HM.empty

buildDirectedGraphT :: [(Vertex, Vertex)] ->  Graph k a
buildDirectedGraphT = buildDirectedGraph . defaultWeight

-- | builds a undirected Graph from a list of 3-Tuples
--
buildUndirectedGraph :: [(Vertex, Vertex, Weight)] -> Graph k a
buildUndirectedGraph = foldl (\a t -> mkGraph addUndirectedEdge [edgeFromTuple t] a) HM.empty

buildUndirectedGraphT :: [(Vertex, Vertex)] -> Graph k a
buildUndirectedGraphT = buildUndirectedGraph . defaultWeight

-- | Helper method to return a given list of Tuples with a default
-- weight of 0
--
defaultWeight :: [(Vertex, Vertex)] -> [(Vertex, Vertex, Weight)]
defaultWeight = fmap (\(x,y) -> (x,y,0))

-- | Creates a graph from a list
-- >>> buildUndirectedGraphT [(1,2)]
-- fromList [(1,[Edge (1,2) 0.0]),(2,[Edge (2,1) 0.0])]
-- >>> buildDirectedGraphT [(1,2)]
-- fromList [(1,[Edge (1,2) 0.0])]
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
addDirectedEdge e@(Edge(x,_) _) = HM.insertWith L.union x [e]


-- | Adds an Edge to the Graph.
--
-- No duplicates are expected if we add the same value twice:
--
addUndirectedEdge :: Edge -> Graph k a -> Graph k a
addUndirectedEdge e@(Edge t w) = addDirectedEdge e . addDirectedEdge swapped
    where swapped = Edge (swap t) w


-- | vertices adjacent to v
adj :: Vertex -> Graph k a -> Maybe [Vertex]
adj v g = Just adjVertices
    where edges = concat . maybeToList $ adjE v g
          adjVertices = fmap extractVertex edges

-- | Edges adjacent to v
--
adjE :: Vertex -> Graph k a -> Maybe [Edge]
adjE = HM.lookup

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
-- >>> let g = buildUndirectedGraphT [(1,2), (1,2), (2, 3), (3, 3), (2, 5)]
-- >>> numV g
-- 4
--
numV :: Graph k a -> Int
numV = HM.foldl' (\a _ -> a + 1) 0


-- | returns the amount of edges
-- >>> let g = buildUndirectedGraphT [(1,2),(2,3),(2,2)]
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
-- >>> countSelfLoops $ buildUndirectedGraphT [(1,2),(2,3),(2,2)]
-- 1
-- >>> countSelfLoops $ buildUndirectedGraphT [(1,2),(1,1),(1,4),(2,2)]
-- 2
--
countSelfLoops :: Graph k a -> Int
countSelfLoops g = foldl (\a k -> a + vCount k) 0 (vertices g)
    where vCount k = foldl (\b y -> if k == y then b + 1 else b) 0 (adjToList k g)


-- | helper to return second vertice from Edge
--
extractVertex :: Edge -> Vertex
extractVertex (Edge(_,y) _) = y
