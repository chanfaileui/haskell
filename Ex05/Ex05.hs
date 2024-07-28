module Ex05 where

import Control.Monad.List
import Control.Monad.State (State, evalState, get, put)
import Data.List
import Debug.Trace
import Test.QuickCheck

-- Task 1. Eight Queens on a Chessboard

-- The queen ♛ is the most powerful piece in the game of chess.
-- Chess is played on an 8x8 checkerboard: each square of the board
-- is labeled by a horizontal and a vertical coordinate ranging from
-- 0 to 7.
--
-- When a queen is placed in one of the squares, it is able to
-- threaten any number of squares vertically, horizontally or
-- diagonally (corner-by-corner) along a straight line.
--
-- For example, the queen Q placed at (1,1) below is able to threaten
-- all the `#` squares, but not the `-` squares.
--
--  0 1 2 3 4 5 6 7
-- 0 # # # - - - - -
-- 1 # Q # # # # # #
-- 2 # # # - - - - -
-- 3 - # - # - - - -
-- 4 - # - - # - - -
-- 5 - # - - - # - -
-- 6 - # - - - - # -
-- 7 - # - - - - - #
--
-- The eight queens puzzle is the problem of placing eight chess
-- queens on the checkerboard in such a way that no two of the
-- queens threaten each other's location.
--
-- In a valid solution of the eight queens problem, no two
-- queens can share the same row, column, or diagonal. Since there
-- are eight columns, each column must therefore contain exactly
-- one queen.
--
-- Thus, we will represent our solution as a list `[Int]`
-- of positions: the first entry of the list tells us the location
-- (a row between 0 and 7) of the queen in column 0,
-- the second entry the location of the queen in column 1, and so on.
--
-- A list of length less than 8 denotes a partial solution, where
-- queens were only placed in the first few columns.

type Row = Int

type Queens = [Row] -- position of queen in each column

type Col = Int

-- 1.a. Implement a function `extend` that takes a single partial solution
--      containing `n` queens in the first `n` columns, and
--      returns a list of all valid extensions of the partial
--      solution by adding a new queen to the `n+1`st column.
--      If there are no such extensions, the function should return [].
--      HINT 1: write a function that determines safe and threatened
--              row positions in the column.
--      HINT 2: you may want to use the list monad.

-- extend :: Queens -> [Queens]
-- extend queens = trace ("Extending: " ++ show queens) $ do
--   let col = length queens
--   row <- [0..7]
--   guard (validRow row col queens)
--   let result = queens ++ [row]
--   trace ("Valid extension: " ++ show [result]) [result]

-- validRow :: Row -> Col -> Queens -> Bool
-- validRow row col queens =
--   all cond (zip queens [0 .. col])
--   where
--     cond (r, c) =
--       let result = row /= r && abs (col - c) /= abs (row - r)
--        in trace ("Checking conflict for position (" ++ show c ++ ", " ++ show r ++ ") with new queen at (" ++ show col ++ ", " ++ show row ++ "): " ++ show result) result

extend :: Queens -> [Queens]
extend queens = do
  let col = length queens
  row <- [0 .. 7]
  guard (validRow row col queens)
  return (queens ++ [row])

validRow :: Row -> Col -> Queens -> Bool
validRow row col queens =
  all (\(r, c) -> row /= r && abs (col - c) /= abs (row - r)) (zip queens [0 .. col])

-- Test cases for the extend function
testExtend1 :: Bool
testExtend1 =
  let queens = []
      expected = [[0], [1], [2], [3], [4], [5], [6], [7]]
   in sort (extend queens) == sort expected

testExtend2 :: Bool
testExtend2 =
  let queens = [1]
      expected = [[1, 3], [1, 4], [1, 5], [1, 6], [1, 7]]
   in sort (extend queens) == sort expected

testExtend3 :: Bool
testExtend3 =
  let queens = [1, 3]
      expected = [[1, 3, 0], [1, 3, 5], [1, 3, 6], [1, 3, 7]]
   in sort (extend queens) == sort expected

testExtend4 :: Bool
testExtend4 =
  let queens = [1, 3, 5]
      expected = [[1, 3, 5, 0], [1, 3, 5, 2], [1, 3, 5, 7]]
   in sort (extend queens) == sort expected

testExtend5 :: Bool
testExtend5 =
  let queens = [1, 3, 5, 0, 4]
      expected = []
   in sort (extend queens) == sort expected

-- Aggregate all tests
runTests :: [Bool]
runTests = [testExtend1, testExtend2, testExtend3, testExtend4, testExtend5]

-- Check if all tests passed
allTestsPassed :: Bool
allTestsPassed = and runTests

-- 1.b. Implement a function that, given a row position `p`,
--      returns the list of all possible solutions to the eight
--      queens problem in which the queen in the first column
--      is placed in the row position `p`.

-- with reference to https://rosettacode.org/wiki/N-queens_problem#Haskell
solutionStartWith :: Int -> [Queens]
solutionStartWith p = foldM (\queens col -> extend queens) [p] [1 .. 7]

-- for any n size: solutionStartWith n = foldM (\queens col -> extend queens) [] [0..n-1]

-- Task 2. RPN calculator
-- Reverse Polish notation (RPN) is a way of writing arithmetic
-- expressions without parentheses. Apart from the lack
-- of parentheses, its main advantage is that it's easy
-- to type into a calculator.
--
-- Handheld RPN calculators were very popular throughout the 80s,
-- and some people still swear by them.
--
-- Generally, evaluation on an RPN calculator proceeds by maintaining
-- a stack of numbers, and handling each user action one at a time.
-- 1. If the user entered a number (e.g. action `Enter 5`),
--    we push the entered number onto a stack.
-- 2. If the user pressed the button for an arithmetic operation,
--    say `+`, we remove the two topmost number `x` and `y` from
--    the stack, and put the result, (in this case `y + x`) on
--    the stack.
-- 3. There are special buttons: `Clear`, which, when pressed, removes
--    the topmost number from the stack; `AllClear`, which removes
--    everything from the stack, and `Swap`, which swaps the two
--    topmost elements on the stack.

-- Old calculators could store only 4-5 elements on their stack.
-- Here, we allow our stack to be an unbounded list of numbers.
-- The bottom of the stack is always padded with zeroes:
-- so the stack `[]` and the stack `[0,0]` behave identically,
-- as the stacks `[5,4]` and `[5,4,0]` behave identically as well.

-- As an example, the following sequence of actions:
-- `[Enter 3, Enter 7, Enter 5, Arith (-), Arith (*)]`
-- will result in the following final stack state: [6].
-- First, 3, 7 and 5 are put on the stack. Then we subtract
-- 5 from 7, leaving 3 and 2 on the stack. Finally, we multiply
-- these two, which yields 6.

type Stack = [Double]

data UserAction
  = Arith (Double -> Double -> Double)
  | Clear
  | AllClear
  | Swap
  | Enter Double

push :: Double -> State Stack ()
push x =
  get >>= \xs ->
    put (x : xs)

-- 2.a. Implement the pop operation which removes and returns
--      the top element of the stack. Keep in mind that empty
--      stacks are treated as if they were padded with zeroes.
pop :: State Stack Double
pop = do
  xs <- get
  case xs of
    [] -> return 0
    (x : xs') -> do
      put xs'
      return x

-- pop :: State Stack Double
-- pop =
--   get >>= \xs ->
--     case xs of
--       [] -> return 0
--       (x : xs') -> put xs' >> return x

-- The Clear user action lets the user remove the top element
-- of the current stack.
clear :: State Stack ()
clear =
  pop >>= \_ ->
    return ()

-- 2.b. Implement the AllClear user action, which removes all
--      elements from the stack.
allClear :: State Stack ()
allClear = get >>= \xs -> put []

-- 2.c. Implement the Swap user action, which switches the order
--      of the two topmost elements in the stack. Keep in mind
--      that empty stacks are zero-padded.
swap :: State Stack ()
swap = do
  x <- pop
  y <- pop
  push x
  push y

-- 2.d. Implement the `Arith` user action, which performs the
--      given arithmetic operation on the top two elements of
--      the stack. E.g. if the stack contains `[2,3]`, then
--      `Arith (-)` should result in `[1]`.
performArith :: (Double -> Double -> Double) -> State Stack ()
performArith arith = do
  x <- pop
  y <- pop
  push (arith y x)

-- 2.e. Implement the `app1` function, which applies the given
--      user action to the current stack.
app1 :: UserAction -> State Stack ()
app1 useraction = do
  case useraction of
    Arith arith -> performArith arith
    Clear -> clear
    AllClear -> allClear
    Swap -> swap
    Enter x -> push x

-- 2.f. Implement the `app` function, which applies the given
--      list of user actions, in order, starting with the head
--      of the list. Once all actions are perforemd, it returns
--      the final state of the stack.
app :: [UserAction] -> State Stack Stack
app actions = do
  mapM_ app1 actions
  get

-- foldM (\_ actions -> app1 actions) []

-- you can use `runCalc` to test your implementation. E.g.

-- * > runCalc [Enter 3, Enter 7, Enter 5, Arith (-), Arith (*)]

-- [6.0]
runCalc :: [UserAction] -> Stack
runCalc xs = evalState (app xs) []

-- Property: Entering a number should put it on top of the stack
prop_Enter :: Double -> Bool
prop_Enter x = evalState (app [Enter x]) [] == [x]

-- Property: Clear should remove the top element of the stack
prop_Clear :: [Double] -> Bool
prop_Clear xs = evalState (app [Clear]) xs == drop 1 xs

-- Property: AllClear should empty the stack
prop_AllClear :: [Double] -> Bool
prop_AllClear xs = null (evalState (app [AllClear]) xs)

-- Property: Swap should swap the top two elements of the stack
prop_Swap :: Double -> Double -> Bool
prop_Swap x y = evalState (app [Enter x, Enter y, Swap]) [] == [x, y]

-- Property: Arith (+) should add the top two elements of the stack
prop_Arith_Add :: Double -> Double -> Bool
prop_Arith_Add x y = evalState (app [Enter x, Enter y, Arith (+)]) [] == [x + y]

-- Property: Arith (-) should subtract the top element from the second top element
prop_Arith_Sub :: Double -> Double -> Bool
prop_Arith_Sub x y = evalState (app [Enter x, Enter y, Arith (-)]) [] == [x - y]

-- Property: Arith (*) should multiply the top two elements of the stack
prop_Arith_Mul :: Double -> Double -> Bool
prop_Arith_Mul x y = evalState (app [Enter x, Enter y, Arith (*)]) [] == [x * y]

-- Property: Arith (/) should divide the second top element by the top element of the stack
prop_Arith_Div :: Double -> Double -> Property
prop_Arith_Div x y = y /= 0 ==> evalState (app [Enter x, Enter y, Arith (/)]) [] == [x / y]

-- Main function to run all QuickCheck properties
main :: IO ()
main = do
  quickCheck prop_Enter
  quickCheck prop_Clear
  quickCheck prop_AllClear
  quickCheck prop_Swap
  quickCheck prop_Arith_Add
  quickCheck prop_Arith_Sub
  quickCheck prop_Arith_Mul
  quickCheck prop_Arith_Div


-- Test cases for the `app` function
testApp1 :: Bool
testApp1 =
  let result = runCalc [Enter 3, Enter 7, Enter 5, Arith (-), Arith (*)]
      expected = [6.0]
   in result == expected

testApp2 :: Bool
testApp2 =
  let result = runCalc [Enter 2, Enter 3, Arith (+), Enter 4, Arith (*)]
      expected = [20.0]
   in result == expected

testApp3 :: Bool
testApp3 =
  let result = runCalc [Enter 10, Enter 4, Enter 3, Arith (-), Arith (/)]
      expected = [10.0]
   in trace (show ("testApp3", result)) (result == expected)

testApp4 :: Bool
testApp4 =
  let result = runCalc [Enter 15, Clear, Enter 5, Enter 3, Arith (+)]
      expected = [8.0]
   in result == expected

testApp5 :: Bool
testApp5 =
  let result = runCalc [Enter 1, Enter 2, Swap, Arith (/)]
      expected = [2.0]
   in result == expected

testApp6 :: Bool
testApp6 =
  let result = runCalc [Enter 3, Enter 5, Enter 8, Enter 5, AllClear]
      expected = []
   in result == expected

testApp7 :: Bool
testApp7 =
  let result = runCalc [Enter 3, Enter 5, Enter 8, AllClear, Enter 1]
      expected = [1.0]
   in result == expected

-- Aggregate all tests
runTestsAll :: [Bool]
runTestsAll = [testApp1, testApp2, testApp3, testApp4, testApp5, testApp6, testApp7]

-- Check if all tests passed
allTestsPassedAll :: Bool
allTestsPassedAll = and runTestsAll

-- Main function to run all tests
main1 :: IO ()
main1 = do
  if allTestsPassedAll
    then putStrLn "All tests passed!"
    else putStrLn "Some tests failed."
