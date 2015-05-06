module Main where

import Control.Monad (unless)
import System.Exit (exitFailure)

import qualified Data.HashMap.Strict as HM
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

import Arbitrary()
import Graph


prop_edge :: Edge -> Bool
prop_edge a = addUndirectedEdge a (addUndirectedEdge a HM.empty) == addUndirectedEdge a HM.empty

main :: IO ()
main = do
  let tests = [ quickCheckResult prop_edge
              ]
  success <- fmap (all isSuccess) . sequence $ tests
  unless success exitFailure
