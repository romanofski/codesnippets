-- Graph using a List: no O(1) lookup
--
module Graph where

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
-- >>> mkGraph [(1, 2), (2, 3), (2, 2)] HM.empty
-- fromList [(1,[2]),(2,[2,3,1]),(3,[2])]
--
mkGraph :: [Edge] -> Graph k a -> Graph k a
mkGraph ([]) g = g
mkGraph (x:xs) g = mkGraph xs (addEdge x g)


-- | vertices adjacent to v
adj :: Vertex -> Graph k a -> Maybe [Vertex]
adj = HM.lookup

-- | adds an Edge to the Graph
addEdge :: Edge -> Graph k a -> Graph k a
addEdge (x,y) g
    | HM.null g = merge y x (HM.singleton x [y])
    | otherwise = merge y x (merge x y g)
    where merge k v = HM.insertWith L.union k [v]


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
