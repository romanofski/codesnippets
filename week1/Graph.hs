module Graph where

-- import Test.QuickCheck
-- import Control.Monad (replicateM)
import Data.Set (Set, insert, empty, fromList)


type Edge = Integer

data Vertex = Vertex (Edge, Edge)
    deriving Show

data Node a = Node (Edge, Set Edge)
    deriving (Show, Eq)

data Graph a = Graph [Node a]
    deriving Show


{-
instance Arbitrary a => Arbitrary (Set a) where
    arbitrary = sized $ \size -> do
        len <- choose (0, size) :: Gen Int
        vals <- replicateM len arbitrary
        return $ fromList vals
-}

-- | Creates a graph from a list
-- >>> mkGraph (Graph []) [(1, 2), (2, 3), (2, 2)]
-- Graph [Node (1,fromList [2]),Node (2,fromList [1,3])]
mkGraph :: Graph a -> [(Edge, Edge)] -> Graph a
mkGraph g ([]) = g
mkGraph g ((x, y):xs) = mkGraph (addEdge g x y) xs

-- | adds an Edge v w to the Graph
addEdge :: Graph a -> Edge -> Edge -> Graph a
addEdge (Graph []) a b                = Graph [Node (a, insert b empty), Node (b, insert a empty)]
addEdge (Graph (Node (a, xs):_)) _ b  = Graph [Node (a, insert b xs)]

-- | returns the amount of vertices
-- >>> let g = mkGraph (Graph []) [(1,2), (2, 3), (3, 3), (2, 5)]
-- >>> numVertices g
-- 4
--
-- Quickcheck property which doesn't work yet
-- length [a] == numVertices [Node a]
numVertices :: Graph a -> Integer
numVertices (Graph []) = 0
numVertices (Graph (Node _:xs)) = 1 + nodeLength xs
    where
        nodeLength :: [Node a] -> Integer
        nodeLength [] = 0
        nodeLength (_:rest) = 1 + nodeLength rest
