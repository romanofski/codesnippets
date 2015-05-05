{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Main where

import Test.DocTest
import Test.QuickCheck
import Control.Monad
import Graph
import Data.Hashable (Hashable)


instance (Arbitrary k, Arbitrary v, Eq k, Hashable k) => Arbitrary (Graph k v) where
    arbitrary = sized $ \size -> do
        len <- choose (0, size)
        vals <- replicateM len (Edge (arbitrary, arbitrary) arbitrary)
        return buildDirectedGraphT $ zip vals vals


instance (Arbitrary Vertex, Arbitrary Weight) => Arbitrary (Edge) where
    arbitrary = do
        a <- arbitrary
        c <- arbitrary
        return $ Edge (a, a) c

main :: IO ()
main = doctest ["-isrc", "src/Main.hs", "src/Graph.hs"]
