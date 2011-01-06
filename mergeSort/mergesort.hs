msort :: [a] -> [a]

msort [] = []
msort (x:[]) = [x]

halflist :: [a] -> ([a], [a])
halflist x = (leftlist, rightlist)
    where middle = (length x) `div` 2
          leftlist = take middle x
          rightlist = drop middle x
