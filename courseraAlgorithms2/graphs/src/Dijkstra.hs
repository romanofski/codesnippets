{-# LANGUAGE TupleSections #-}
module Dijkstra (
    Dijkstra(..)
  , getShortestPath) where

import qualified Data.HashMap.Strict as HM
import qualified Data.PQueue.Prio.Min as Queue
import Control.Lens ((&), lens, Lens', set, at, view, non, over, hasn't)

import Graph
-- $setup
-- >>> let udg = buildUndirectedGraph [(0,1,100.0),(0,2,30.0),(1,5,300.0),(5,4,50.0),(5,6,70.0),(4,6,150.0),(4,7,30.0),(4,3,80.0),(7,6,150.0),(7,3,90.0),(2,3,200.0)]
-- >>> let d = makeDijkstra 0 udg

data Dijkstra = Dijkstra
    { _toVisit :: Queue.MinPQueue Weight Vertex
    , _distScore :: HM.HashMap Vertex Double
    } deriving (Show)

toVisit :: Lens' Dijkstra (Queue.MinPQueue Weight Vertex)
toVisit = lens _toVisit (\d x -> d { _toVisit = x})

distScore :: Lens' Dijkstra (HM.HashMap Vertex Double)
distScore = lens _distScore (\d x -> d { _distScore = x})

getShortestPath :: Vertex -> Vertex -> Graph -> Maybe Double
getShortestPath source goal g =
  view (distScore . at goal) (runSearch (makeDijkstra source g) g)

makeDijkstra :: Vertex -> Graph -> Dijkstra
makeDijkstra start g =
  let initScore = HM.fromList $ (,infinity) <$> vertices g
      open = Queue.singleton 0 start
      infinity = 100000
  in set (distScore . at start) (Just 0) (Dijkstra open initScore)

-- | Runs the Dijkstra search recursively and terminates if we visited all nodes.
--
-- TODO: Note, the use of the queue operation is non-total. It is preferred to
-- use either a better data structure with error handling or catch the error and
-- terminate with an `Either String Dijkstra` type.
--
-- >>> view (distScore . at 5) (runSearch d udg)
-- Just 360.0
runSearch :: Dijkstra -> Graph -> Dijkstra
runSearch d g =
    let ((_, bestVertex),queue) = Queue.deleteFindMin (view toVisit d)
        neighbours = adjE bestVertex g
        d' = foldr calcLength (set toVisit queue d) neighbours
    in if hasn't (toVisit . traverse) d'
           then d'
           else runSearch d' g

-- |
-- >>> let d1 = calcLength  (Edge (0, 1) 100) d
-- >>> view (distScore . at 1) d1
-- Just 100.0
-- >>> let d2 = calcLength (Edge (0, 2) 30.0) d1
-- >>> view (distScore . at 0) d2
-- Just 0.0
-- >>> view (distScore . at 2) d2
-- Just 30.0
-- >>> let d3 = calcLength (Edge (0, 1) 100.0) d2
-- >>> view (distScore . at 0) d3
-- Just 0.0
-- >>> view (distScore . at 1) d3
-- Just 100.0
calcLength :: Edge -> Dijkstra -> Dijkstra
calcLength (Edge (x, y) weight) d =
  let alt = view (distScore . at x . non 0) d + weight
      distv = view (distScore . at y . non 0) d
  in if alt < distv then
       d & set (distScore . at y) (Just alt)
       . over toVisit (Queue.insert alt y)
     else d
