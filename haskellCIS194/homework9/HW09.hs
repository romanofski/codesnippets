{-# LANGUAGE RankNTypes #-}
import Test.QuickCheck
import HW05
import Ring
import Control.Monad (liftM4)


main :: IO ()
main = do
  quickCheck (propRing :: Mod5 -> Mod5 -> Mod5 -> Property)
  quickCheck (propRing :: Mat2x2 -> Mat2x2 -> Mat2x2 -> Property)

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
prop_associative :: (Eq a, Ring a) => a -> a -> a -> Bool
prop_associative a b c = (a `add` b) `add` c == a `add` (b `add` c)

prop_addId :: (Eq a, Ring a) => a -> Bool
prop_addId x = x `add` addId == x

prop_addInv :: (Eq a, Ring a) => a -> Bool
prop_addInv x = x `add` (addInv x) == addId

prop_commutative :: (Eq a, Ring a) => a -> a -> Bool
prop_commutative a b = a `add` b == b `add` a

prop_isMonoid :: (Eq a, Ring a) => a -> a -> a -> Bool
prop_isMonoid a b c = (a `mul` b) `mul` c == a `mul` (b `mul` c)

prop_mulId :: (Eq a, Ring a) => a -> Bool
prop_mulId a = a `mul` mulId == a

prop_leftDistributivity :: (Eq a, Ring a) => a -> a -> a -> Bool
prop_leftDistributivity a b c = a `mul` (b `add` c) == (a `mul` b) `add` (a `mul` c)

prop_rightDistributivity :: (Eq a, Ring a) => a -> a -> a -> Bool
prop_rightDistributivity a b c = (b `add` c) `mul` a == (b `mul` a) `add` (c `mul` a)

-- | Exercise 3
--
-- prop> propRing a b (c :: Mod5)
-- prop> propRing a b (c :: Mat2x2)
propRing :: (Eq a, Ring a) => a -> a -> a -> Property
propRing a b c = conjoin [
      prop_addId a
    , prop_addInv a
    , prop_mulId a
    , prop_associative a b c
    , prop_commutative a b
    , prop_isMonoid a b c
    , prop_leftDistributivity a b c
    , prop_rightDistributivity a b c
    ]
