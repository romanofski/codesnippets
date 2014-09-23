main :: IO ()
main = print $ show $ dropLastDigit 12345


-- basically a `last` on a given integer
-- Problems:
--  * character to (Int to ) Integer
-- |
-- >>> lastDigit 123
-- 3
-- >>> lastDigit 3
-- 3
-- >>> lastDigit 53234
-- 4
lastDigit :: Integer -> Integer
lastDigit x =  x `mod` 10


-- |
-- >>> dropLastDigit 5
-- 0
-- >>> dropLastDigit 1234
-- 123
-- >>> dropLastDigit 1235
-- 123
dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10


-- Converts a number into a list of digits
-- |
-- >>> toDigits 1234
-- [1,2,3,4]
-- >>> toDigits 0
-- []
-- >>> toDigits (-17)
-- []
toDigits :: Integer -> [Integer]
toDigits x
    | x <= 0 = []
    | otherwise = t ++ [l] where
              l = lastDigit x
              t = toDigits $ dropLastDigit x


-- Double every second digit from the right
-- |
-- >>> doubleEveryOther [8,7,6,5]
-- [16,7,12,5]
-- >>> doubleEveryOther [1,2,3]
-- [1,4,3]
-- >>> doubleEveryOther [1]
-- [1]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ doubleEveryOther' $ reverse xs


doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' [] = []
doubleEveryOther' [x] = [x]
doubleEveryOther' (x:(y:zs)) = x : (y * 2) : doubleEveryOther' zs
