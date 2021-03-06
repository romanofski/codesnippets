-- Graph using a List: no O(1) lookup
--
-- Note for improvements: This code started in 2015. There are quite a bit of
-- improvements to be made to make the usage much more flexible. First of, we
-- should investigate to use more types for the different types of graphs in
-- order to distinguish adding edges to each type. Speaking of edges, also those
-- should use types to differentiate between weighted and un-weighted edges.
--
module Graph where

import Data.Tuple (swap)
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Test.QuickCheck (Arbitrary, arbitrary)


-- | Edge connecting two vertices
--
data Edge = Edge (Vertex, Vertex) Weight
    deriving (Show, Eq)

instance Arbitrary Edge where
  arbitrary = Edge <$> arbitrary <*> arbitrary

type Vertex = Int
type Weight = Double

type Graph = HM.HashMap Vertex  -- Vertex
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
buildDirectedGraph :: [(Vertex, Vertex, Weight)] -> Graph
buildDirectedGraph = foldl (\a t -> mkGraph addDirectedEdge [edgeFromTuple t] a) HM.empty

buildDirectedGraphT :: [(Vertex, Vertex)] ->  Graph
buildDirectedGraphT = buildDirectedGraph . defaultWeight

-- | builds a undirected Graph from a list of 3-Tuples
--
buildUndirectedGraph :: [(Vertex, Vertex, Weight)] -> Graph
buildUndirectedGraph = foldl (\a t -> mkGraph addUndirectedEdge [edgeFromTuple t] a) HM.empty

buildUndirectedGraphT :: [(Vertex, Vertex)] -> Graph
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
mkGraph :: (Edge -> Graph -> Graph)     -- insertion function
           -> [Edge]                            -- list of edges to insert
           -> Graph
           -> Graph
mkGraph _ ([]) g = g
mkGraph f (x:xs) g = mkGraph f xs (f x g)


-- | Addes an Edge to a Graph creating a directed or symmetric directed
-- graph.
--
addDirectedEdge :: Edge -> Graph -> Graph
addDirectedEdge e@(Edge(x,_) _) = HM.insertWith L.union x [e]


-- | Replaces the given edge. If the edge is not found no change is carried out
-- on the graph
-- >>> let g = buildUndirectedGraph [(1,2,10.0),(2,3,20.0),(3,1,30.0),(2,2,0.0)]
-- >>> replaceUndirectedEdge (Edge (1,2) 5.0) g
-- fromList [(1,[Edge (1,2) 5.0,Edge (1,3) 30.0]),(2,[Edge (2,1) 5.0,Edge (2,2) 0.0,Edge (2,3) 20.0]),(3,[Edge (3,1) 30.0,Edge (3,2) 20.0])]
replaceUndirectedEdge :: Edge -> Graph -> Graph
replaceUndirectedEdge e = HM.unionWith (replaceEdges e) (addUndirectedEdge e HM.empty)

replaceEdges :: Edge -> [Edge] -> [Edge] -> [Edge]
replaceEdges e@(Edge t w) new old = new `L.union` filterEdges swapped (filterEdges e old)
  where
    swapped = Edge (swap t) w

-- |
-- >>> filterEdges (Edge (1,2) 1.0) [(Edge (1,2) 5.0), (Edge (1,3) 2.0)]
-- [Edge (1,3) 2.0]
-- >>> filterEdges (Edge (1,2) 1.0) [(Edge (2,2) 5.0), (Edge (2,3) 2.0)]
-- [Edge (2,2) 5.0,Edge (2,3) 2.0]
filterEdges :: Edge -> [Edge] -> [Edge]
filterEdges e = filter (not . comp e)
  where
    comp (Edge (x, y) _) (Edge (x', y') _) = x == x' && y == y'

-- | Adds an Edge to the Graph.
--
-- No duplicates are expected if we add the same value twice:
--
-- prop> addUndirectedEdge a (addUndirectedEdge a HM.empty) == addUndirectedEdge a HM.empty
addUndirectedEdge :: Edge -> Graph -> Graph
addUndirectedEdge e@(Edge t w) = addDirectedEdge e . addDirectedEdge swapped
    where swapped = Edge (swap t) w


-- | vertices adjacent to v
adj :: Vertex -> Graph -> Maybe [Vertex]
adj v g = Just adjVertices
    where edges = adjE v g
          adjVertices = fmap extractVertex edges

-- | Edges adjacent to v
--
adjE :: Vertex -> Graph -> [Edge]
adjE v g = fromMaybe [] $ HM.lookup v g

-- | safe helper to convert Maybe adj to list. [] is returned in case no
-- neighbours exist.
--
adjToList :: Vertex -> Graph -> [Vertex]
adjToList k g = concat $ maybeToList $ adj k g

-- | Vertices of the Graph
--
vertices :: Graph -> [Vertex]
vertices = HM.keys

-- | returns the amount of vertices
-- >>> let g = buildUndirectedGraphT [(1,2), (1,2), (2, 3), (3, 3), (2, 5)]
-- >>> numV g
-- 4
--
numV :: Graph -> Int
numV = HM.foldl' (\a _ -> a + 1) 0


-- | returns the amount of edges
-- >>> let g = buildUndirectedGraphT [(1,2),(2,3),(2,2)]
-- >>> numE g == length (HM.toList g)
-- True
--
numE :: Graph -> Int
numE = HM.size


-- | degree of a vertice
--
degree :: Vertex -> Graph -> Int
degree x g =
    case adj x g of
        Just vs -> foldl (\a _ -> a + 1) 0 vs
        Nothing -> 0

-- | max degree
--
maxDegree :: Graph -> Int
maxDegree g = maximum $ fmap length vals
    where vals = mapMaybe (`adj` g) (HM.keys g)

-- | average degree
--
avgDegree :: Graph -> Double
avgDegree g = 2.0 * (e / v)
    where e = fromIntegral $ numE g
          v = fromIntegral $ numV g


-- | count self loops
-- >>> countSelfLoops $ buildUndirectedGraphT [(1,2),(2,3),(2,2)]
-- 1
-- >>> countSelfLoops $ buildUndirectedGraphT [(1,2),(1,1),(1,4),(2,2)]
-- 2
--
countSelfLoops :: Graph -> Int
countSelfLoops g = foldl (\a k -> a + vCount k) 0 (vertices g)
    where vCount k = foldl (\b y -> if k == y then b + 1 else b) 0 (adjToList k g)


-- | helper to return second vertice from Edge
--
extractVertex :: Edge -> Vertex
extractVertex (Edge(_,y) _) = y
