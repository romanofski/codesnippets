import Data.Char


main :: IO ()
main = print $ show $ dropLastDigit 12345


-- basically a `last` on a given integer
-- Problems:
--  * character to (Int to ) Integer
lastDigit :: Integer -> Integer
lastDigit x = toInteger $ digitToInt $ last $ show x

dropLastDigit :: Integer -> Integer
dropLastDigit x
    | x < 10     = 0
    | otherwise  = read $ init $ show x :: Integer
