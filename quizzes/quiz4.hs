-- Q1

-- {-# LANGUAGE DeriveFunctor #-}

-- data Tree a = Leaf | Branch a (Tree a) (Tree a)
--   deriving (Functor)

-- instance Show a => Show (Tree a) where
--   show :: Show a => Tree a -> String
--   show Leaf = "Leaf"
--   show (Branch x left right) = "Branch " ++ show x ++ " (" ++ show left ++ ") (" ++ show right ++ ")"

-- -- Example usage
-- exampleTree :: Tree Int
-- exampleTree = Branch 1 (Branch 2 Leaf Leaf) (Branch 3 Leaf Leaf)

-- -- fmap example
-- mappedTree :: Tree String
-- mappedTree = fmap show exampleTree

-- main :: IO ()
-- main = print mappedTree

{-# LANGUAGE DeriveFunctor #-}

data NonEmptyList a = One a | Cons a (NonEmptyList a)
  deriving (Functor)

-- Define the Show instance for NonEmptyList
instance Show a => Show (NonEmptyList a) where
  show (One x) = "One " ++ show x
  show (Cons x xs) = "Cons " ++ show x ++ " (" ++ show xs ++ ")"

-- Example non-empty list
exampleList :: NonEmptyList Int
exampleList = Cons 1 (Cons 2 (One 3))

-- Use fmap to increment each element
incrementedList :: NonEmptyList Int
incrementedList = fmap (+1) exampleList

main :: IO ()
main = print incrementedList
