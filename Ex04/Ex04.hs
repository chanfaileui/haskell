module Ex04 where

import Control.Monad.State (State, evalState, get, put)
import Data.Monoid
import Data.Semigroup
import Priority
import Size
import Test.QuickCheck

-- DEFINITIONS AND HELPER FUNCTIONS --

type NodeInfo = (Size, Priority)

data QueueTree a
  = Null
  | Leaf NodeInfo a
  | Node NodeInfo (QueueTree a) (QueueTree a)
  deriving (Show)

nodeInfo :: QueueTree a -> NodeInfo
nodeInfo Null = mempty
nodeInfo (Leaf i _) = i
nodeInfo (Node i _ _) = i

sizeOf :: QueueTree a -> Size
sizeOf = fst . nodeInfo

maxPrio :: QueueTree a -> Priority
maxPrio = snd . nodeInfo

-- checks whether the tree structure
-- is balanced (i.e. that the left subtree and the right
-- subtree don't ever differ too much in size)
balanced :: QueueTree a -> Bool
balanced (Node i l r) =
  let sl = unSize (sizeOf l)
   in let sr = unSize (sizeOf r)
       in abs (sl - sr) <= 1 && balanced l && balanced r
balanced _ = True

-- EXERCISE STARTS HERE --

-- Task 1a. Write a well-formedness predicate
--          wf for the `QueueTree` data type.

-- Hint: Both `Priority` and `Size` are semigroups/monoids.
-- This means that the type `NodeInfo` is also automatically
-- a monoid.

-- well formed: correct priority and correct size
wf :: QueueTree a -> Bool
wf Null = True
wf (Leaf (size, priority) _) = unSize size == 1
wf (Node (size, priority) left right) =
  size == sizeOf left <> sizeOf right
    && priority == maxPrio left <> maxPrio right
    && wf left
    && wf right

-- Task 1b. Write smart constructors `leaf` and `node`
--          for the `QueueTree` data type which maintain
--          the well-formedness invariant. I.e. given
--          well-formed inputs, the smart constructors
--          should give well-formed outputs.
--          You should /not/ tweak the structure of the ~QueueTree~
--          beyond updating the ~NodeInfo~; in particular don't do
--          ~node Null Null = Null~.

leaf :: Priority -> a -> QueueTree a
leaf priority = Leaf (size 1, priority)

node :: QueueTree a -> QueueTree a -> QueueTree a
node left right = Node (nodeInfo left <> nodeInfo right) left right

-- Task 2a. Implement the usual priority queue functions
--          for the type `QueueTree`. These are
--          pop - Remove the element from the queue that has the
--               highest priority. Return the modified queue,
--               along with the removed element (if any).
--          insert - add an element to the queue with the given priority.
pop :: QueueTree a -> (QueueTree a, Maybe a)
pop Null = (Null, Nothing)
pop (Leaf _ a) = (Null, Just a)
pop (Node _ left right) =
  if unPriority (maxPrio left) >= unPriority (maxPrio right)
    then
      -- pop left
      let (newLeft, popped) = pop left
       in (node newLeft right, popped)
    else
      -- pop right
      let (newRight, popped) = pop right
       in (node left newRight, popped)

insert :: Priority -> a -> QueueTree a -> QueueTree a
insert p x Null = leaf p x
insert p x (Leaf (size, priority) y) = node (leaf p x) (Leaf (size, priority) y)
insert p x (Node _ left right) =
  if unSize (sizeOf left) >= unSize (sizeOf right)
    then node (insert p x left) right
    else node left (insert p x right)

-- Task 2b. Implement a function `fromList` that converts a
--          list of `(Priority, x)` pairs into a well-formed
--          and balanced `QueueTree x` structure.

fromList :: [(Priority, a)] -> QueueTree a
fromList [] = Null
fromList ((p, x) : xs) = insert p x (fromList xs)

-- or alternatively:
-- fromList = foldr (uncurry insert) Null
-- uncurry makes sure insert can receive (Priority, a) as a pair
-- (instead of as Priority -> a, basically currying)
-- foldr folds/maps the list from right to left

-- Hint: you can use `fromList` to implement an `Arbitrary`
-- instance for `QueueTree`, allowing you to test your work.

-- Task 3. Implement stateful versions of the pop and insert
--         operations above using the `State` type in Haskell's
--         standard mtl library.
--         Implement a `peek` operation which just returns the
--         highest-priority element without changing the
--         state of the queue.
--         Do not use the `state` function in your final
--         implementations!
pop' :: State (QueueTree a) (Maybe a)
pop' = do
  tree <- get
  let (newTree, popped) = pop tree
  put newTree
  return popped

insert' :: Priority -> a -> State (QueueTree a) ()
insert' priority x = do
  tree <- get
  put (insert priority x tree)

peek' :: State (QueueTree a) (Maybe a)
peek' = do
  tree <- get
  return $ snd (pop tree)

-- END OF EXERCISE --

-- You can use the following three examples to test your
-- implementations of pop' and insert', and to practice
-- reading `State`-ful functions.

-- Returns the highest priority currently in the `QueueTree`
-- without changing the state.
getMaxPrio' :: State (QueueTree a) Priority
getMaxPrio' =
  get >>= \q ->
    return (maxPrio q)

-- Removes the element with the second-highest priority
-- in the queue.
dip' :: State (QueueTree a) ()
dip' =
  getMaxPrio' >>= \p ->
    pop' >>= \h1 ->
      pop' >>= \h2 ->
        case h1 of
          Nothing -> return ()
          Just h1 -> insert' p h1

-- a `State`-free version of dip
dip :: QueueTree Char -> QueueTree Char
dip =
  evalState $
    dip' >>= \() ->
      get
