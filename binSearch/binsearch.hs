binsearch :: Ord a => a -> [a] -> Int
binsearch x []   = error "Item not in list or empty list"
binsearch x [y]  = 1
binsearch x xs = search x 0 (length xs) xs

-- search
-- returns a tuple with (element, index)
-- we always return a middle, even if the length is uneven
search :: Ord a => a -> Int -> Int -> [a] -> Int
search e i j []  = error "empty list"
search e i j [x] = 0
search e i j xs
    | e > li = search e m l xs
    | e < li = search e 0 m xs
    | otherwise = i
        where m = (i + ((j - i) `div` 2))
              l = length xs
              li = head (drop m xs)
