module Main where

import Graph
import Test.QuickCheck
import Arbitrary()
import qualified Data.HashMap.Strict as HM


prop_edge a = addUndirectedEdge a (addUndirectedEdge a HM.empty) == HM.empty

main :: IO ()
main = do
  quickCheck prop_edge
