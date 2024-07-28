-- Ex1: Create a function rev that reverses a list
rev :: [a] -> [a]
rev = foldl (\acc x -> x : acc) []
-- Alternate: 
-- rev = foldl (flip (:)) []

-- rev [] = []
-- rev (x:xs) = foldr(\x acc -> acc ++ [x]) [] (x:xs)

-- Ex2: Create a function prefixes that returns a list of all prefixes of a list
prefixes :: [a] -> [[a]]
prefixes = scanl (\acc x -> acc ++ [x]) []

-- Tries? https://www.youtube.com/watch?v=46dksIrx6jQ&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV&index=11