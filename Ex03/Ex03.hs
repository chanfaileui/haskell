module Ex03 where

import Data.List
import Test.QuickCheck

-- Consider the following six properties of the nub function:
--
-- applying it twice gives the same result
-- Idempotence: 
-- 1. nub (nub xs) == nub xs

-- applying it to subset of list is the same as applying it to the whole list
-- 2. nub (x : nub xs) == nub (x : xs)

-- Appending a list ys after applying nub to it and then applying nub to the 
-- whole result is the same as appending ys to xs and applying nub once to the entire result.
-- This ensures that duplicates are removed no matter where they appear.
-- 3. nub (xs ++ nub ys) == nub (xs ++ ys)

-- consecutive duplicates are removed by nub.
-- 4. nub (x : x : xs) == nub (x : xs)

-- duplicates introduced by the second copy of xs are removed.
-- 5. nub (xs ++ [x] ++ xs) = nub (xs ++ [x])

-- nub has no effect on a single-element list.
-- 6. nub [x] == [x]

-- Task 1. Implement a dodgy nub function that satisfies
-- equations 1, 2, 3 and 6, but not 4, 5.
-- Give counterexamples to properties 4,5, i.e. values
-- (x,xs) for which equations 4 and 5 do not hold.

dnub1236 :: (Eq a) => [a] -> [a]
dnub1236 = error "dnub1236 not defined"

dnub1236_c4 :: (Int, [Int]) -- (x,xs) failing eqn 4
dnub1236_c4 = error "dnub1236_c4 not defined"

dnub1236_c5 :: (Int, [Int]) -- (x,xs) failing eqn 5
dnub1236_c5 = error "dnub1236_c5 not defined"

-- Task 2. Implement a dodgy nub function that satisfies
-- equations 1, 2, 3, 4, 5, but not 6.
-- Give counterexamples.

dnub12345 :: (Eq a) => [a] -> [a]
dnub12345 xs = error "dnub12345 not defined"

dnub12345_c6 :: Int
dnub12345_c6 = error "dnub12345_c6 not defined"

-- Task 3. Implement a dodgy nub function that satisfies
-- equations 4, 5, 6, but not 1,2,3.
-- Give counterexamples.

dnub456 :: (Eq a) => [a] -> [a]
dnub456 xs = error "dnub456 not defined"

dnub456_c1 :: [Int]
dnub456_c1 = error "dnub456_c1 not defined"

dnub456_c2 :: (Int,[Int])
dnub456_c2 = error "dnub456_c2 not defined"

dnub456_c3 :: ([Int],[Int]) -- (xs,ys)
dnub456_c3 = error "dnub456_c3 not defined"

-- Task 4. Implement a dodgy nub function that satisfies
-- all the equations 1,2,3,4,5,6. (3 marks)

dnub123456 :: (Eq a) => [a] -> [a]
dnub123456 xs = error "dnub123456 not defined"

-- Show that dnub123456 is dodgy, i.e. give an input xs for which
-- dnub123456 xs /= nub xs.
dnub123456_c :: [Int]
dnub123456_c = error "dnub123456_c not defined"

