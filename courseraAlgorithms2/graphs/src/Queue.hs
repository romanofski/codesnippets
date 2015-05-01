-- based on Chris Okasaki thesis
--
module Queue where

data Queue a = Queue [a] [a]
    deriving Show

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _             = False


-- | enqueue
--
enq :: a -> Queue a -> Queue a
enq y (Queue xs ys) = Queue xs (y:ys)

-- | dequeue
--
-- >>> let q = Queue [] []
-- >>> deq q
-- (Nothing,Queue [] [])
-- >>> enq 1 q
-- Queue [] [1]
-- >>> deq $ Queue [] [1]
-- (Just 1,Queue [] [])
deq :: Queue a -> (Maybe a, Queue a)
deq q@(Queue [] []) = (Nothing, q)
deq (Queue (x:xs) ys) = (Just x, Queue xs ys)
deq (Queue [] ys) = deq (Queue (reverse ys) [])
