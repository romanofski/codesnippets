import Text.Read
import Data.Maybe


-- |
-- >>> parseContents "13 14"
-- [(13, 14)]
parseContents :: String -> Maybe (Int, Int)
parseContents xs = lstToTuple $ strToList xs
    where strToList str = mapMaybe readMaybeInt (words str)

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe

lstToTuple :: [Int] -> Maybe (Int, Int)
lstToTuple [x, y] = Just (x, y)
lstToTuple _ = Nothing

-- | read a data file which is supposed to be structure like:
--
--  13
--  11
--  12 12
--  1 2
--
-- to build our graph. The presumption here is to build graphs in which
-- the Vertices are Integers.
--
main :: IO ()
main = do
    contents <- getContents
    let parsed = mapMaybe parseContents (lines contents)
    print parsed
