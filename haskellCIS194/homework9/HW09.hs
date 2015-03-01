{-# LANGUAGE RankNTypes #-}
import Test.QuickCheck
import HW05
import Ring (Ring, add, addId)
import Control.Monad (replicateM)


-- | Exercise 1
--
instance Arbitrary Mod5 where
    arbitrary = sized $ \size -> do
        n <- choose (0, toInteger size)
        return $ MkMod (n `mod` 5)

instance Arbitrary Mat2x2 where
    arbitrary =  do
        list <- replicateM 4 arbitrary
        return $ Mat2x2 1 2 3 4

prop_addId :: Mod5 -> Bool
prop_addId x = x `add` addId == x

prop_addIdM :: Mat2x2 -> Bool
prop_addIdM x = x `add` addId == x
