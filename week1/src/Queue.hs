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
-- >>> pop q
-- Nothing
-- >>> push 1 q
-- Queue [] [1]
-- >>> pop $ Queue [] [1]
-- Just (1,Queue [] [])
pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue (x:xs) ys) = Just (x, Queue xs ys)
pop (Queue [] ys) = pop (Queue (reverse ys) [])
