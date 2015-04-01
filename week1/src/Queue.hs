-- based on Chris Okasaki thesis
--
module Queue where

data Queue a = Queue [a] [a]
    deriving Show

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _             = False


push :: a -> Queue a -> Queue a
push y (Queue xs ys) = Queue xs (y:ys)


-- | enqeue
--
-- >>> let q = Queue [] []
-- >>> deq q
-- Nothing
-- >>> push 1 q
-- Queue [] [1]
-- >>> deq $ Queue [] [1]
-- Just (1,Queue [] [])
deq :: Queue a -> Maybe (a, Queue a)
deq (Queue [] []) = Nothing
deq (Queue (x:xs) ys) = Just (x, Queue xs ys)
deq (Queue [] ys) = deq (Queue (reverse ys) [])
