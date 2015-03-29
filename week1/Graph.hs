-- Graph using a List: no O(1) lookup
--
module Graph where

import Data.Maybe
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L

type Edge = (Vertex, Vertex)

type Vertex = Int

type Graph k a = HM.HashMap Vertex    -- Vertex
                            [Vertex]  -- Neighbours XXX not a set, so keeping it free of duplicates is a performance hit


{-
instance Arbitrary a => Arbitrary (Set a) where
    arbitrary = sized $ \size -> do
        len <- choose (0, size) :: Gen Int
        vals <- replicateM len arbitrary
        return $ fromList vals
-}

-- | Creates a graph from a list
-- >>> mkGraph [(1,2)] HM.empty
-- fromList [(1,[2]),(2,[1])]
--
mkGraph :: [Edge] -> Graph k a -> Graph k a
mkGraph ([]) g = g
mkGraph (x:xs) g = mkGraph xs (addEdge x g)


-- | Adds an Edge to the Graph. Note, the order we add does matter. It
-- matters when we traverse the graph in depth first order, since
-- children (neighbours) are traversed by the order we retrieve them.
-- This is most of the time the vertices were added to the adjacency
-- list.
--
-- >>> mkGraph [(0,1),(0,2)] HM.empty
-- fromList [(0,[2,1]),(1,[0]),(2,[0])]
--
addEdge :: Edge -> Graph k a -> Graph k a
addEdge (x,y) g
    | HM.null g = merge y x (HM.singleton x [y])
    | otherwise = merge y x (merge x y g)
    where merge k v = HM.insertWith L.union k [v]


-- | vertices adjacent to v
adj :: Vertex -> Graph k a -> Maybe [Vertex]
adj = HM.lookup


-- | returns the amount of vertices
-- >>> let g = mkGraph [(1,2), (1,2), (2, 3), (3, 3), (2, 5)] HM.empty
-- >>> numV g
-- 4
--
numV :: Graph k a -> Int
numV = HM.foldl' (\a _ -> a + 1) 0


-- | returns the amount of edges
-- >>> let g = mkGraph [(1,2),(2,3),(2,2)] HM.empty
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
-- >>> countSelfLoops $ mkGraph [(1,2),(2,3),(2,2)] HM.empty
-- 1
-- >>> countSelfLoops $ mkGraph [(1,2),(1,1),(1,4),(2,2)] HM.empty
-- 2
--
countSelfLoops :: Graph k a -> Int
countSelfLoops = HM.foldlWithKey' (\a k v -> if k `elem` v then a + 1 else a) 0


-- | depth first search
-- Find a path from a given to a goal Vertex
--
-- >>> let g =  mkGraph [(0,5),(4,3),(0,1),(6,4),(5,4),(0,2),(0,6),(5,3)] HM.empty
-- >>> dfs 0 [] [] g
-- [0,6,2,1,5,0,4]
dfs :: Vertex    -- start node
    -> [Vertex]  -- nodes to_visit
    -> [Vertex]  -- nodes visited
    -> Graph k a -- the graph
    -> [Vertex]  -- returned visited
dfs x [] [] g = dfs x (getChildren x g []) [x] g
dfs _ [] vs _ = vs
dfs _ (x:xs) vs g
    | x `elem` vs = vs
    | otherwise = dfs next (getChildren x g xs) (vs ++ [x]) g
    where next = head xs

getChildren :: Vertex -> Graph k a -> [Vertex] -> [Vertex]
getChildren k g xs = xs ++ to_visit
    where to_visit = concat $ maybeToList $ adj k g
