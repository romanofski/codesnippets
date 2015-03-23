-- Graph using a List: no O(1) lookup
--
module Graph where

-- import Test.QuickCheck
-- import Control.Monad (replicateM)
import Data.Vector
import Prelude hiding (length)


type Edge = Integer

data Node a = Empty
            | Vertex Edge (Node a)
    deriving (Show, Eq)

data Graph a = Graph (Vector (Node a))
    deriving Show


{-
instance Arbitrary a => Arbitrary (Set a) where
    arbitrary = sized $ \size -> do
        len <- choose (0, size) :: Gen Int
        vals <- replicateM len arbitrary
        return $ fromList vals
-}

-- | Creates a graph from a list
-- >>> mkGraph [(1, 2), (2, 3), (2, 2)] (Graph [])
-- Graph [Node (1,fromList [2]),Node (2,fromList [1,3])]
mkGraph :: [(Integer, Integer)] -> Graph a -> Graph a
mkGraph ([]) g = g
mkGraph ((x, y):xs) g = mkGraph xs (addEdge (Vertex x (Vertex y Empty)) g)

-- | adds a Vertex to the Graph
-- prop> addEdge x g == addEdge x (addEdge x g)
addEdge :: Node a -> Graph a -> Graph a
addEdge x@(Vertex _ (Vertex _ Empty)) (Graph xs) = Graph (x `cons` xs)
addEdge x@(Vertex a b) (Graph xs) = case findEdge x xs of
    Just (Vertex _ _) -> Graph xs
    Nothing -> Graph (x `cons` xs)
-- addEdge (Graph (Node (Vertex a, xs):_)) _ b  = Graph [Node (a, insert b xs)]

findEdge :: Node a -> Vector (Node a) -> Maybe (Node a)
findEdge x = find (== x)

-- | returns the amount of vertices
-- >>> let g = mkGraph (Graph []) [(1,2), (2, 3), (3, 3), (2, 5)]
-- >>> numVertices g
-- 4
--
-- Quickcheck property which doesn't work yet
-- length [a] == numVertices [Node a]
numVertices :: Graph a -> Int
numVertices (Graph xs) = length xs
