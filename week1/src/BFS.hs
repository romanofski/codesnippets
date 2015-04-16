module BFS where

import Graph
import Data.List
import Control.Monad.State
import qualified Queue as Q

-- | breadth first search
-- Find a path from a given to a goal Vertex
--
-- We re-use the same test graph as for the dfs implementation.
-- >>> let g =  mkGraph addUndirectedEdge [(0,5),(4,3),(0,1),(6,4),(5,4),(0,2),(0,6),(5,3)] HM.empty
-- >>> bfs 0 g
-- [0,6,2,1,5,4,3]
--
-- >>> let g =  mkGraph addDirectedEdge [(0,5),(4,3),(0,1),(6,4),(5,4),(0,2),(0,6),(5,3)] HM.empty
-- >>> bfs 0 g
-- [0,6,2,1,5,4,3]
--
bfs :: Vertex -> Graph k a -> [Vertex]
bfs x = go [x] (Q.Queue [x][])
        where go vs (Q.Queue [] []) _ = vs
              go vs q g = go vs' q' g
                where (vs', q') = runState (bfsSearchChildren siftChildren vs g) q

-- | breadth first search helper
-- This function takes a monadic function which queues all
-- unvisited nodes, and then marks the queued, unvisited nodes as
-- visited.
--
bfsSearchChildren :: ([Vertex] -> [Vertex] -> State (Q.Queue Vertex) [Vertex])
                     -> [Vertex]
                     -> Graph k a
                     -> State (Q.Queue Vertex) [Vertex]
bfsSearchChildren f vs g = state Q.deq >>= \x -> case x of
        Just y -> f (adjToList y g) vs
        Nothing -> return []

-- | Breadth First Search Helper
-- Returns a new list of visited nodes, while having enqueued all
-- unvisited nodes.
--
-- >>> let q = Q.Queue [] []
-- >>> evalState (siftChildren [] [1,2]) q
-- [1,2]
-- >>> runState (siftChildren [1,2] []) q
-- ([1,2],Queue [] [2,1])
-- >>> runState (siftChildren [1,2] [2]) q
-- ([2,1],Queue [] [1])
--
siftChildren :: Eq a =>
                [a]                                -- children
                -> [a]                             -- visited nodes
                -> State (Q.Queue a) [a]      -- queue in state monad
siftChildren [] vs = return vs
siftChildren xs vs = do
    let unvisited = xs \\ vs
    mapM_ statefulEnq unvisited
    return $ vs `union` unvisited
    where statefulEnq x = void $ state (\q -> ((), Q.enq x q))
