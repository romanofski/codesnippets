{-# LANGUAGE RankNTypes #-}
import Test.QuickCheck
import HW05
import Ring
import Control.Monad (liftM4)


main :: IO ()
main = do
  quickCheck (prop_addId :: Mod5 -> Bool)
  quickCheck (prop_addId :: Mat2x2 -> Bool)
  quickCheck (prop_associative :: Mod5 -> Mod5 -> Mod5 -> Bool)
  quickCheck (prop_associative :: Mat2x2 -> Mat2x2-> Mat2x2 -> Bool)
  quickCheck (prop_addInv :: Mod5 -> Bool)
  quickCheck (prop_addInv :: Mat2x2 -> Bool)
  quickCheck (prop_commutative :: Mod5 -> Mod5 -> Bool)
  quickCheck (prop_commutative :: Mat2x2 -> Mat2x2 -> Bool)
  quickCheck (prop_isMonoid :: Mod5 -> Mod5 -> Mod5 -> Bool)
  quickCheck (prop_isMonoid :: Mat2x2 -> Mat2x2 -> Mat2x2 -> Bool)
  quickCheck (prop_mulId :: Mod5 -> Bool)
  quickCheck (prop_mulId :: Mat2x2 -> Bool)
  quickCheck (prop_leftDistributivity :: Mod5 -> Mod5 -> Mod5 -> Bool)
  quickCheck (prop_leftDistributivity :: Mat2x2 -> Mat2x2 -> Mat2x2 -> Bool)
  quickCheck (prop_rightDistributivity :: Mod5 -> Mod5 -> Mod5 -> Bool)
  quickCheck (prop_rightDistributivity :: Mat2x2 -> Mat2x2 -> Mat2x2 -> Bool)

-- | Exercise 1
--
instance Arbitrary Mod5 where
    arbitrary = sized $ \size -> do
        n <- choose (0, toInteger size)
        return $ MkMod (n `mod` 5)

instance Arbitrary Mat2x2 where
    arbitrary = liftM4 Mat2x2 arbitrary arbitrary arbitrary arbitrary

-- | Exercise 2
--
prop_associative a b c = (a `add` b) `add` c == a `add` (b `add` c)

prop_addId x = x `add` addId == x

prop_addInv x = x `add` (addInv x) == addId

prop_commutative a b = a `add` b == b `add` a

prop_isMonoid a b c = (a `mul` b) `mul` c == a `mul` (b `mul` c)

prop_mulId a = a `mul` mulId == a

prop_leftDistributivity a b c = a `mul` (b `add` c) == (a `mul` b) `add` (a `mul` c)

prop_rightDistributivity a b c = (b `add` c) `mul` a == (b `mul` a) `add` (c `mul` a)
