module Main where
import Prelude hiding (elem)

main :: IO ()
main = putStrLn "Hello, world!"

-- https://www.youtube.com/watch?v=Cxkqrg8FCt8&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV&index=5
-- Ex1: Create a function elem that returns True if an element is in a list and False otherwise
elem :: (Eq a) => a -> [a] -> Bool
elem _ [] = False -- could've also given the variable a name
elem e (x:xs) = e == x || elem e xs

-- Ex2: Create a function nub that removes all duplicates from a given list
-- nub :: (Eq a) => [a] -> [a]
-- nub [] = []
-- nub (x:xs) = 
--     | nub [] = []
--     | x = head(xs)
--     | x:xs

nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x:xs)
    | x `elem` xs = nub xs
    | otherwise   = x : nub xs

-- alternate:
nub' :: Eq a => [a] -> [a]
nub' (x : xs) = x : nub' (filter (/= x) xs)

-- Ex3: Create a function isAsc that returns True if the list is sorted in ascending order and False otherwise
-- isAsc :: [Int] -> Bool
-- isAsc [] = True
-- isAsc [_] = True
-- isAsc (x:xs) = aux n 1
--     | x > 1 = 
--     | otherise = 

isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [x] = True
isAsc (x:y:xs) = x <= y && isAsc (y:xs)

-- alternate:
isAs :: [Int] -> Bool
isAs [x] = True    -- single element
isAs (x:xs) = x <= head xs && isAsc xs    -- r ecursion


-- Ex4: Create a function hasPath that determines if a path exists between two nodes in a directed graph
hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath [] x y = x == y -- no edges more to look at -> check if starting node is the end node
hasPath xs x y 
    | x == y = True -- if there are edges, if the starting node is the end node, return True
    | otherwise =
        let xs' = [(a, b) | (a, b) <- xs, a /= x] in -- prepare the list of edges to be passed to the recursive call
                                                     -- all the tuples from xs where a does not equal x
                                                     -- delete all the edges we just took, this is to avoid cycles
        or [hasPath xs' b y | (a, b) <- xs, a == x]  -- for each edge that starts at x, check if there is a path from b to y
                                                     -- if there is a path from b to y, return True
                                                     -- if there is no path from b to y, return False



