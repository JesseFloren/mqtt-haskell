module Utils.Queue where
import Control.Concurrent (MVar, newEmptyMVar, putMVar, newMVar, takeMVar, readMVar)

data Queue a = Queue a (Queue a) | End

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