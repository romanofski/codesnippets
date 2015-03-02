{-# LANGUAGE RankNTypes #-}
import Test.QuickCheck
import HW05
import Ring (Ring, add, addId)
import Control.Monad (liftM4)


main :: IO ()
main = do
  quickCheck prop_addIdM
  quickCheck prop_addId

-- | Exercise 1
--
instance Arbitrary Mod5 where
    arbitrary = sized $ \size -> do
        n <- choose (0, toInteger size)
        return $ MkMod (n `mod` 5)

instance Arbitrary Mat2x2 where
    arbitrary =  do
        mat <- liftM4 Mat2x2 arbitrary arbitrary arbitrary arbitrary
        return $ mat

prop_addId :: Mod5 -> Bool
prop_addId x = x `add` addId == x

prop_addIdM :: Mat2x2 -> Bool
prop_addIdM x = x `add` addId == x
