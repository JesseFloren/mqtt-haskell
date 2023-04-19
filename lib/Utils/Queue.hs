module Utils.Queue where

data Queue a = Queue a (Queue a) | End deriving (Show, Ord, Eq)

instance Functor Queue where
    fmap _ End = End
    fmap f (Queue x xs) =  Queue (f x) (fmap f xs)

instance Foldable Queue where
  foldr _ b End = b
  foldr f b (Queue x xs) = f x $ foldr f b xs 

empty :: Queue a
empty = End

single :: a -> Queue a
single x = Queue x End

push :: a -> Queue a -> Queue a
push x End = Queue x End
push x (Queue a q) = Queue a (push x q)

pop :: Queue a -> (Maybe a, Queue a)
pop End = (Nothing, End)
pop (Queue x q) = (Just x, q)

peek :: Queue a -> (Maybe a, Queue a)
peek End = (Nothing, End)
peek (Queue x q) = (Just x, Queue x q)