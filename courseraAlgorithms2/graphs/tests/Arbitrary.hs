{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Arbitrary where

import Test.QuickCheck
import Control.Monad
import Graph


instance (Arbitrary k, Arbitrary v) => Arbitrary (Graph k v) where
    arbitrary = sized $ \size -> do
        len <- choose (0, size)
        vals <- replicateM len arbitrary
        return $ buildDirectedGraphT $ zip vals vals


instance (Arbitrary Vertex, Arbitrary Weight) => Arbitrary (Edge) where
    arbitrary = do
        a <- arbitrary
        c <- arbitrary
        return $ Edge (a, a) c

