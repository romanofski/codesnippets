msort :: [a] -> [a]

msort [] = []
msort (x:[]) = [x]


midlist :: [a] -> [a]
midlist x = take middle x
    where middle = (length x) `div` 2
