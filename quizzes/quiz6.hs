module Quiz6 where

import Data.Char (isDigit)

data Direction = L | R

forward :: IO ()
forward = putStrLn "Moving forward"

obstructed :: IO Bool
obstructed = undefined

turn :: Direction -> IO ()
turn L = putStrLn "Turning left"
turn R = putStrLn "Turning right"

robot :: IO b
robot = do
  sensed <- obstructed
  if sensed
    then turn L
    else forward
  robot

b :: IO ()
b = do
  x <- getLine
  putStrLn (filter (not . isDigit) x)
  b

a1 :: IO ()
a1 = getLine >>= putStrLn . filter (not . isDigit) >> a1

a2 :: IO ()
a2 = getLine >>= \x -> putStrLn (filter (not . isDigit) x) >>= \_ -> a2

-- a3 :: IO ()
-- a3 = (getLine >>= \x -> putStrLn (filter (not . isDigit) x)) >>= a3

-- a4 :: IO ()
-- a4 = do getLine >>= \x -> putStrLn . filter (not . isDigit); a4

-- a5 :: IO ()
-- a5 = do x <- getLine; putStrLn . filter (not . isDigit); a5

a6 :: IO ()
a6 = do x <- getLine; putStrLn . filter (not . isDigit) $ x; a6

a7 :: IO ()
a7 = do getLine >>= \x -> putStrLn (filter (not . isDigit) x); a7

-- return empty string
-- f1 [] = return ""
-- f1 (x:xs) = putChar x >>= \_ -> xs

-- f2 [] = return ()
-- f2 (x : xs) = putChar x >>= \_ -> f2 xs

-- f3 [] = return ()
-- f3 (x : xs) = putChar x >>= \xs -> f3 (show xs)

-- f4 xs = case xs of
--   [] -> return ()
--   (x : xs) -> f4 xs >>= \_ -> putChar x


mapIO :: (a -> b) -> IO a -> IO b
-- mapIO f m = return (f m)
-- mapIO f m = m >>= \x -> return (f x)
-- mapIO f m = m >>= \x -> f x
-- mapIO f = (f <$>)

hundredsum = foldl (++) [] [[x * x] | x <- [1 .. 100]]

hundredsum2 = sum [x ^ 2 | x <- [1 .. 100]]

hundredsum3 = sum [const x x | x <- [1 .. 100]]

hundredsum4 = sum (map (^ 2) [1 .. 100])