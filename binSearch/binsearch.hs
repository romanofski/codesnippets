binsearch :: Ord a => a -> [a] -> Int
binsearch x []   = error "Empty list"
binsearch x [y]  = 0
binsearch x xs = case (x `elem` xs) of True -> search x 0 (length xs) xs
                                       False -> error "Element not in list"

-- search
-- It takes the element e, lower i and upper boundary j of the list xs
-- and returns the index of the given element e.
search :: Ord a => a -> Int -> Int -> [a] -> Int
search e i j []  = error "empty list"
search e i j [x] = 0
search e i j xs
    | e > li = search e m l xs
    | e < li = search e 0 m xs
    | otherwise = m
        where m = (i + ((j - i) `div` 2))
              l = length xs
              li = head (drop m xs)
