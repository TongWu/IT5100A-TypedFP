module Section3Support where

-- | The 'State' monad as discussed in class.
newtype State s a = State { runState :: s -> (a, s) }

-- Basic functions for working with 'State'
put :: s -> State s ()
put s = State $ const ((), s)

get :: State s s 
get = State $ \s -> (s, s)

modify :: (s -> s) -> State s ()
modify f = do s <- get
              put (f s)

-- Basic type class instances.
instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f (State f') = State $ 
        \s -> let (a, s') = f' s
              in  (f a, s')

instance Applicative (State s) where
    pure :: a -> State s a
    pure x = State (x,)
    (<*>) :: State s (a -> b) -> State s a -> State s b
    (<*>) (State f) (State x) = State $ 
        \s -> let (f', s') = f s
                  (x', s'') = x s'
              in  (f' x', s'')

instance Monad (State s) where
    return :: a -> State s a
    return = pure
    (>>=) :: State s a -> (a -> State s b) -> State s b
    (State f) >>= m = State $ 
        \s -> let (a, s') = f s
                  State f' = m a
              in  f' s'

