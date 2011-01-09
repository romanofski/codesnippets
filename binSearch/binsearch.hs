binsearch :: Ord a => a -> [a] -> Int
binsearch x []   = error "Item not in list or empty list"
binsearch x [y]  = 1
binsearch x xs
    | x > m = binsearch x (slicelist i l xs)
    | x < m = 2
    | otherwise = i
        where (m, i) = getmiddle xs


slicelist :: Int -> Int -> [a] -> [a]
slicelist a b [] = []
slicelist a b [x] = [x]
slicelist a b xs = drop a (take b xs)

-- get
-- returns an element e by given index i from a list
get :: Int -> [a] -> a
get i [] = error "empty list"
get i [x] = x
get i xs = head (slicelist n i xs)
    where n = i - 1

-- getmiddle
-- returns a tuple with (element, index)
-- we always return a middle, even if the length is uneven
getmiddle :: [a] -> (a, Int)
getmiddle []  = error "empty list"
getmiddle [x] = (x, 0)
getmiddle xs  = (x, i)
    where i = (length xs) `div` 2
          x = head (drop i xs)
