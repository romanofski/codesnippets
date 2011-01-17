import qualified Test.HUnit

-- merge sort
-- 1. if the list has only one element, it's sorted
-- 2. split the list into two lists in the middle and reapply merge sort
-- 3. sort each two lists by comparing the elements into a merged list
mergesort :: (Ord a) => [a] -> [a]
mergesort []     = error "Can't sort an empty list"
mergesort (x:[]) = [x]
mergesort x      = mergelist (mergesort leftlist, mergesort rightlist)
    where h = halflist x
          leftlist = fst h
          rightlist = snd h


mergelist :: (Ord a) => ([a], [a]) -> [a]
mergelist ([], []) = []
mergelist (xs, []) = xs
mergelist ([], ys) = ys
mergelist (left@(x:xs), right@(y:ys))
    | x < y = [x] ++ mergelist (xs, right)
    | otherwise = [y] ++ mergelist (left, ys)


halflist :: [a] -> ([a], [a])
halflist l = (left, right)
    where middle = (length l) `div` 2
          left   = take middle l
          right  = drop middle l

-- tests
test_halflist = Test.HUnit.TestCase (Test.HUnit.assertEqual "halflist" (halflist [1,3,4]) ([1], [3,4]))
test_mergesort = Test.HUnit.TestCase (Test.HUnit.assertEqual "mergesort" (mergesort [42,38,5,12,22]) [5,12,22,38,42])
tests = Test.HUnit.TestList [Test.HUnit.TestLabel "test halflist" test_halflist,
                             Test.HUnit.TestLabel "test mergesort" test_mergesort]

testrunner = Test.HUnit.runTestTT tests
