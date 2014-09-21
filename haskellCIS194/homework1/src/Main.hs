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
lastDigit x =  abs (r - x)
    where r = round (toRational x * 0.1) * 10


-- |
-- >>> dropLastDigit 5
-- 0
-- >>> dropLastDigit 1234
-- 123
-- >>> dropLastDigit 1235
-- 123
dropLastDigit :: Integer -> Integer
dropLastDigit x
    | x < 10     = 0
    | otherwise  = floor y
    where y = toRational x * 0.1


-- Converts a number into a list of digits
-- |
-- >>> toDigits 1234
-- [1,2,3,4]
-- >>> toDigits 0
-- []
-- >>> toDigits (-17)
-- []
toDigits
