module Quiz5 where

import Control.Applicative

-- fmap :: Monad m => (a -> b) -> m a -> m b
-- fmap f xs =
--   xs >>= \x -> return (f x)

-- pair :: (Applicative f) => f a -> f b -> f (a, b)
-- pair fa fb = Prelude.fmap (,) fa <*> fb

-- s :: Monad m => [m a] -> m [a]
-- s [] = return []
-- s (a:as) = do
--   a
--   s as
--   return (a : as)

data NonEmptyList a = One a | Cons a (NonEmptyList a) 

instance Functor NonEmptyList where
  fmap f (One x) = One (f x)
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
instance Applicative NonEmptyList where
  pure x = Cons x (pure x)
  One f <*> xs = fmap f xs 
  (Cons f fs) <*> xs = fmap f xs `append` (fs <*> xs)
    where
     append (One x) ys = Cons x ys
     append (Cons x xs) ys = Cons x (xs `append` ys)