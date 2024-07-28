module MoveGenerator where

{- Feel free to import more stuff here, as long as it's all from
   `base`. In other words, importing it should not require tinkering
   with the stack.yaml or .cabal files.
 -}

import Control.Monad ((>=>))
import Data.Char (toLower)
import Data.List (groupBy, intersperse, nub, sort, permutations)
import Data.Maybe (fromMaybe, mapMaybe, isNothing)
import Test.QuickCheck
import Debug.Trace (trace)

{- A dictionary, for our purposes, is a set of words.
   A word, for our purposes is a String.

   A Trie is a data structure for representing dictionaries.  Its main
   advantage is that many operations, such as lookup and insert, have
   runtime that's proportional to the length of the word, not the size
   of the dictionary.

   A node `Trie b ts` consists of:
   - `b::Bool` indicating whether the empty word is included in the
     dictionary or not.
   - A list `ts::[(Char,Trie)]` of pairs `(x,t)` of characters `x`
     and associatied subtries `t`. The intention is that
     x:xs is in the dictionary `Trie b ts` whenever `xs` is in the
     dictionary `t`.
     In a well-formed Trie, these lists must be sorted by character,
     and cannot contain duplicate entries for any character.

   For example, the dictionary ["he","hell","hello","hi"]
   would be represented by the following Trie:

                False
                  | 'h'
                False
          'e'  /     \ 'i'
              /       \
           True       True
             | 'l'
           False
             | 'l'
           True
             | 'o'
           True

   ...which looks like this in our Haskell representation:

   Trie False
     [('h',
       Trie False
         [('e',
           Trie True
             [('l',
              Trie False
               [('l',
                Trie True
                  [('o',
                    Trie True [])
                  ]
                )]
          )]
      ),
      ('i',
       Trie True [])])]
 -}
data Trie = Trie Bool [(Char, Trie)] deriving (Eq, Show)

{- `empty` represents an empty dictionary. -}
empty :: Trie
empty = Trie False []

{- `single xs` represents a dictionary consisting of only `xs`. -}
single :: String -> Trie
single [] = Trie True []
single (x : xs) = Trie False [(x, single xs)]

{- `insert t xs` inserts the word xs into the dictionary t. -}
insert :: String -> Trie -> Trie
insert [] (Trie _ ts) = Trie True ts
insert (x : xs) (Trie b ts) =
  case span ((< x) . fst) ts of -- splits the list of children ts into two parts,
  -- where ts1 is char less than x
    (ts1, []) -> Trie b $ ts1 ++ [(x, single xs)] -- ts2 empty, x not found, append x
    (ts1, (y, t) : ts2)
      | x == y -> Trie b $ ts1 ++ (x, insert xs t) : ts2 -- x is alr a child, recursively append subtree
      | otherwise -> Trie b $ ts1 ++ (x, single xs) : (y, t) : ts2 -- x is inserted before (y, t)

{- `toList t` gives the list of all words in t.
   If `t` is well-formed, they will come out in alphabetical order.
 -}
toList :: Trie -> [String]
toList (Trie b ts) =
  first ++ rest
  where
    first
      | b = [""] -- True, return empty strinf
      | otherwise = [] -- False, return nothing
    rest = concatMap (\(x, t) -> map (x :) $ toList t) ts -- recrusively map the first element
    -- and concatenate the resulting lists

{- `fromList ws` should return a dictionary containing
   exactly the words in ws.
 -}
fromList :: [String] -> Trie
fromList = foldr insert empty

-- Properties
prop_fromListContains :: [String] -> Bool
prop_fromListContains ws = all (contains (fromList ws)) ws -- check if all words in ws are in the Trie

prop_notContainsIfNotInList :: [String] -> String -> Property
prop_notContainsIfNotInList ws w = notElem w ws ==> not (contains (fromList ws) w) -- check if a word not in ws is not in the Trie

-- Helper function to check if a Trie contains a given string
contains :: Trie -> String -> Bool
contains (Trie isEnd _) [] = isEnd -- if the Trie is the end of a word, return True
contains (Trie _ ts) (x : xs) = case lookup x ts of -- lookup the first character in the list of children
  Nothing -> False -- if not found, return False
  Just t -> contains t xs -- if found, recursively check the rest of the string

{- Recall that a Trie is well-formed if all the
   [(Char,Trie)] lists in it have the following
   properties:
   - they are sorted in ascending order by the Char
   - they contain at most one entry for each Char.

  Write a predicate `wellFormed` that returns True if
  the given Trie is well-formed, and False otherwise.

  Note that you are responsible for maintaining
  well-formedness: all functions you write that produce
  tries should return a well-formed trie, if their
  argument tries (if any) are all well-formed.
 -}

wellFormed :: Trie -> Bool
wellFormed (Trie _ ts) =
  isOrderedUnique (map fst ts) -- char is unique and sorted
    && all (wellFormed . snd) ts -- recursively check the subtries

isOrderedUnique :: (Ord a) => [a] -> Bool
isOrderedUnique xs = xs == nub (sort xs)

-- Properties
prop_insertContains :: String -> Trie -> Bool
prop_insertContains word trie = contains (insert word trie) word

prop_wellFormedAfterInsert :: String -> Trie -> Property
prop_wellFormedAfterInsert word trie =
  wellFormed trie ==> wellFormed (insert word trie)

prop_fromListWellFormed :: [String] -> Bool
prop_fromListWellFormed words = wellFormed (fromList words)

{- We say that a trie is *minimal* if it contains no
   dead branches. A dead branch is a subtrie containing no
   words. (As a special case, `empty` counts as minimal).

   Here is a minimal trie and a non-minimal trie,
   both representing the same dictionary.

       False               False
         | 'h'       'h' /     \ 'k'
       True            True    False
                  'a'  /   \ 'i'
                    False False

       ^^Minimal      ^^Not Minimal

   The non-minimal tree above has three dead branches.
   The only way to make it minimal is to cut off all three,
   obtaining the tree on the left.

   Write a predicate `minimal` that returns True if
   the given Trie is minimal, and False otherwise.

   Note that well-formedness and minimality are orthogonal:
   it's possible for a Trie to be minimal and not well-formed,
   or vice versa.
 -}
minimal :: Trie -> Bool
minimal (Trie b ts) =
  let
    head = True
    leaves = all (\(_,t) -> isMinimal t) ts
  in
    head && leaves

isMinimal :: Trie -> Bool
isMinimal (Trie b ts) =
  let
    head = (Trie b ts /= empty)
    leaves = all (\(_,t) -> isMinimal t) ts
  in
    head && leaves

{- Write a `prune t` which returns a minimal
   trie representing the same dictionary as t.
 -}
prune :: Trie -> Trie
prune (Trie b subtries) = Trie b (pruneSubtries subtries) -- prune on just subtries

pruneSubtries :: [(Char, Trie)] -> [(Char, Trie)]
pruneSubtries [] = []
pruneSubtries ((c, t) : rest) =
  let prunedSubTrie = prune t
   in if isDeadBranch prunedSubTrie
        then pruneSubtries rest
        else (c, prunedSubTrie) : pruneSubtries rest

isDeadBranch :: Trie -> Bool
isDeadBranch (Trie False []) = True
isDeadBranch _ = False

-- Properties
prop_pruneIdempotent :: Trie -> Bool
prop_pruneIdempotent t = prune (prune t) == prune t

prop_pruneMinimal :: Trie -> Bool
prop_pruneMinimal t = minimal (prune t)

{- Here's a generator and associated
   Arbitrary instance for use with QuickCheck.
   This should only generate well-formed Tries,
   but is not guaranteed to generate minimal Tries.

   It's *not* necessary to fully understand what's
   going on here.
 -}
genTrie :: Int -> Gen Trie
genTrie 0 = pure $ Trie True [] -- when n = 0, return empty trie
genTrie n =
  Trie <$> arbitrary <*> (genKeys >>= genSubtries) -- when n > 0, return trie with arbitrary bool and subtries
  where
    genKeys :: Gen [Char]
    genKeys = sort . nub <$> (resize 5 . listOf $ elements ['a' .. 'z'])
    genSubtries :: [Char] -> Gen [(Char, Trie)]
    genSubtries cs =
      zip cs <$> vectorOf (length cs) (genTrie . max 0 $ n - 1 - length cs) -- zip the keys with the subtries

instance Arbitrary Trie where
  arbitrary = sized $ genTrie . min 15 -- limit the size of the trie to 15
  shrink (Trie b ts) =
    (Trie b <$> shrinkList (const []) ts)
      ++ (Trie b <$> map shrink ts) -- shrink the subtries

{- `check t xs` should return True if `xs` is
   in the dictionary `t`, and False otherwise.
 -}

-- ref: https://stackoverflow.com/questions/33188258/haskell-find-tuple-with-first-element-x-in-list-of-tuples
check :: Trie -> String -> Bool
check (Trie b subtries) [] = b
check (Trie _ subtries) (x : xs) =
  case lookup x subtries of
    Just subtrie -> check subtrie xs
    Nothing -> False

{- The union of two dictionaries t,t' should contain
   all words that occur in either t or t'.
 -}
union :: Trie -> Trie -> Trie
-- union t1 empty = t1
-- union empty t2 = t2
union (Trie b1 ts1) (Trie b2 ts2) = Trie (b1 || b2) (mergeTries ts1 ts2)

mergeTries :: [(Char, Trie)] -> [(Char, Trie)] -> [(Char, Trie)]
mergeTries [] ts = ts
mergeTries ts [] = ts
mergeTries ((c1, t1) : ts1) ((c2, t2) : ts2)
  | c1 < c2 = (c1, t1) : mergeTries ts1 ((c2, t2) : ts2) -- if c1 < c2, append c1 and t1 to the result
  | c1 > c2 = (c2, t2) : mergeTries ((c1, t1) : ts1) ts2 -- if c1 > c2, append c2 and t2 to the result
  | otherwise = (c1, t1 `union` t2) : mergeTries ts1 ts2 -- if c1 == c2, append c1 and union of t1 and t2 to the result

{- The intersection of two dictionaries t,t' should contain
   all words that occur in *both* t and t'.
 -}
intersection :: Trie -> Trie -> Trie
intersection (Trie b1 ts1) (Trie b2 ts2) =
  let intersectTrie (x, t) =
        case lookup x ts2 of
          Nothing -> []
          Just t' -> dropEmpty (x, intersection t t') -- recursively intersect the subtries
      dropEmpty (_, Trie False []) = [] -- drop empty tries
      dropEmpty t = [t] -- return the trie if not empty
   in Trie (b1 && b2) (concatMap intersectTrie ts1)

-- Properties
-- Property: Union with empty should return the original Trie
prop_unionWithEmpty :: Trie -> Bool
prop_unionWithEmpty t = (t `union` empty == t) && (empty `union` t == t)

-- Property: Union should contain all words from both Tries
prop_unionContainsAllWords :: Trie -> Trie -> [String] -> Bool
prop_unionContainsAllWords t1 t2 = all (\w -> check (t1 `union` t2) w == (check t1 w || check t2 w))

-- Property: Intersection with empty should be empty
prop_intersectionWithEmpty :: Trie -> Bool
prop_intersectionWithEmpty t = (t `intersection` empty == empty) && (empty `intersection` t == empty)

-- Property: Intersection should contain only common words from both Tries
prop_intersectionContainsCommonWords :: Trie -> Trie -> [String] -> Bool
prop_intersectionContainsCommonWords t1 t2 = all (\w -> check (t1 `intersection` t2) w == (check t1 w && check t2 w))

{- One of the above (union or intersection) forms
   a monoid with `empty` as identity element.
   Use the right one to define the following monoid
   instance:
 -}
newtype TrieMonoid = TrieMonoid {fromMonoid :: Trie} deriving (Eq, Show)

instance Semigroup TrieMonoid where
  (<>) (TrieMonoid t1) (TrieMonoid t2) = TrieMonoid (t1 `union` t2)

instance Monoid TrieMonoid where
  mappend = (<>)
  mempty = TrieMonoid empty

{- In the remainder of the assignment, we will use our Trie library
   above to develop a move generator for Scrabble-like word games.

   A player makes a *move* by placing a sequence of tiles on the
   board, either horizontally or vertically. These tiles will
   connect with the pre-existing letters on the board to form
   words. A move is *legal* if every word thus formed occurs
   in the dictionary.

   A move generator is a core component for any word game program.
   It takes the following inputs:
   - A Trie, representing the dictionary
   - An Int, representing the number of tiles the player will place.
     To find all moves, we would need to run the move generator once for
     every possible number of tiles, and combine the results.
   - A Rack. This represents the player's pool of letter tiles
     available for play.
   - A Board. This represents the current state of the board.
   It produces the following output:
   - A Trie, representing the subset of the dictionary that are legal
     legal moves of the desired length.

   We make a number of simplifying assumptions:
   - We will only generate moves that start from a particular fixed square
     on the board.
   - We only consider horizontal moves.
 -}

{- pick x xs is a utility function which should satisfy the following properties:

   - If `elem x xs`, then `pick xs x = (True,ys)` for some `ys`
                     such that `xs` is a permutation of `x:ys`.
   - If `not(elem x xs)`, then `pick xs x = (False,ys)`, for some `ys`
                          such that `xs` is a permutation of `ys`.

   This utility function is useful for pulling out specific tiles from a rack.
 -}
pick :: (Eq a) => [a] -> a -> (Bool, [a])
pick [] _ = (False, [])
pick (x : xs) y
  | x == y = (True, xs)
  | otherwise = let (b, ys) = pick xs y in (b, x : ys)

{- `sandwichableLetters t xs ys` should return a list
   containing all the characters x such that
   `xs++x:ys` is a word in `t`.

   This will be handy for collecting constraints imposed by words
   formed vertically.
 -}
sandwichableLetters :: Trie -> String -> String -> [Char]
sandwichableLetters (Trie b ts) xs ys =
  [x | x <- nub (concat (toList (Trie b ts))), check (Trie b ts) (xs ++ [x] ++ ys)]

{- A constraint represents a predicate on characters.
   A character c is said to *match* a constraint
   according to the following clauses:

   - Any character c matches `Wildcard`.
   - A character c matches `Mem cs`, if c occurs in cs.
 -}
data Constraint = Wildcard | Mem String deriving (Show, Eq)

{- QuickCheck generator for constraints.
   Not necessary to follow all the details. -}
instance Arbitrary Constraint where
  arbitrary =
    oneof
      [ pure Wildcard, -- Always generates Wildcard
        Mem . sort . nub <$> listOf (elements ['a' .. 'z'])
      ]
  shrink Wildcard = []
  shrink (Mem xs) = Mem <$> shrinkList (: []) xs

{- A Pattern is a list of Constraints.
   We say that a word xs matches a pattern cs if:
   - `length xs == length cs`
   - The i:th character of xs matches the i:th constraint in cs
     for all i.

   This is a simplified form of regular expressions, whose full
   generality we will not need here.
 -}
type Pattern = [Constraint]

{- `filterLength n t` should return
   a dictionary containing all words
   in t that have length n.
 -}
filterLength :: Int -> Trie -> Trie
filterLength len = filterLengthHelper 0
  where
    filterLengthHelper :: Int -> Trie -> Trie
    filterLengthHelper depth (Trie end ts)
      | depth == len = Trie end []
      | depth < len = Trie False (mapMaybe filterSubTrie ts)
      | otherwise = Trie False []
      where
        filterSubTrie (ch, subTrie) =
          let filteredSubTrie = filterLengthHelper (depth + 1) subTrie
           in if isNonEmptyTrie filteredSubTrie
                then Just (ch, filteredSubTrie)
                else Nothing

    isNonEmptyTrie :: Trie -> Bool
    isNonEmptyTrie (Trie end ts) = end || not (null ts)

-- Tests
-- print $ filterLength 2 exampleTrie
-- exampleTrie :: Trie
-- exampleTrie = Trie False
--   [ ('a', Trie False
--       [ ('b', Trie True
--           [ ('c', Trie True [])
--           ]
--         )
--       ]
--     )
--   , ('d', Trie False
--       [ ('e', Trie True [])
--       ]
--     )
--   ]
-- tree = fromList ["a", "hi", "hot", "rate", "mango"]
-- hci> filterLength 3 tree

{- `filterPattern cs t` should return
   a dictionary containing all words in t
   that matches the pattern cs.
 -}
isMatch :: Char -> Constraint -> Bool
isMatch _ Wildcard = True
isMatch char (Mem s) = char `elem` s

patternMatch :: String -> Pattern -> Bool
patternMatch xs cs =
  let
    lengthMatch = length xs == length cs
    allMatch = all (uncurry isMatch) (zip xs cs)
  in
    lengthMatch && allMatch

filterPattern :: Pattern -> Trie -> Trie
filterPattern p trie = fromList (filter (`patternMatch` p) (toList trie))

-- Test
-- toList (filterPattern [Mem ['c'], Mem ['a'], Mem['t']] csw)
-- toList (filterPattern [Mem ['c'], Mem ['a'], Wildcard] csw)

{- A Tile is either a letter tile, or a blank tile.
   Blank tiles are the most OP thing in the game:
   they are wildcards that can be played as any
   letter.

   A Rack is just a list of Tiles..
 -}
data Tile = Letter Char | Blank deriving (Eq, Show)

type Rack = [Tile]

{- QuickCheck generator for tiles.
   Very biased towards Blank tiles. -}
instance Arbitrary Tile where
  arbitrary =
    oneof
      [ pure Blank,
        Letter <$> elements ['a' .. 'z']
      ]

{- `filterPlayables r t` should return
   all the words in t that can be formed
   by using (a subset of) the tiles in the
   rack r.

   Note that each tile can only be used once.

   For example, the tiles [Letter 'w', Blank, Letter 'o']
   can form many English words, including:

    we, ow, cow, how, who, of, wow, ...

   But not including, e.g.

     whom

   Which would require using the Blank twice.

   Of course, the above words are just an example: they need not be
   present in the input dictionary.
 -}
filterPlayables :: Rack -> Trie -> Trie
filterPlayables rack trie = fromList (filter (isWordPlayable rack) (toList trie))

isWordPlayable :: Rack -> String -> Bool
isWordPlayable rack word = any (isPlayableWithRack word) (permutations rack)

isPlayableWithRack :: String -> Rack -> Bool
isPlayableWithRack [] _ = True
isPlayableWithRack _ [] = False
isPlayableWithRack (c:cs) rack =
  let remainingRack = removeTileFromRack c rack
  in case remainingRack of
      Just r' -> isPlayableWithRack cs r'
      Nothing -> False

removeTileFromRack :: Char -> Rack -> Maybe Rack
removeTileFromRack _ [] = Nothing
removeTileFromRack c (tile:rack) =
  case tile of
    Letter letter -> if c == letter then Just rack else (tile :) <$> removeTileFromRack c rack
    Blank         -> Just rack

-- -- Test
-- exampleTrie :: Trie
-- exampleTrie = Trie False
--   [ ('a', Trie False
--       [ ('b', Trie True [])
--       , ('c', Trie False
--           [ ('d', Trie True [])
--           ]
--         )
--       ]
--     )
--   , ('e', Trie False
--       [ ('f', Trie True [])
--       ]
--     )
--   ]

-- exampleRack :: Rack
-- exampleRack = [Letter 'a', Blank, Letter 'd']

{- `Board xs` represents a view of the board state as follows:
   - each element of xs represents a column
   - the first element of xs is the column where the player's first
     tile is placed.
   - the second element of xs represents the column immediately to the
     right of the column where the player's first tile is placed,
     and so on.
   - The contents of each column is represented by a tuple `((as,bs),r)`
     as follows:
     - r represents the contents of the row where the player makes their
       move (the *main row*), as follows:
       - if `r == Nothing`, there is no pre-existing tile in this square.
       - if `r == Just c`, there is a pre-existing tile with letter `c`
         in this square. Additional tiles can't be stacked on top,
         but the player can build around it.
     - `as` represents the longest contiguous sequence of letters
       immediately above the main row, from top to bottom.
       If `as = []`, it means the square above the main row is empty.
     - `bs` represents the longest contiguous sequence of letters
       immediately below the main row, from top to bottom.
       If `bs = []`, it means the square above the main row is clear.
   - The board does not extend beyond xs. Words that would go beyond
     the board are illegal moves and cannot be played.
 -}
newtype Board = Board [((String, String), Maybe Char)] deriving (Eq, Show)

{- `moves t n r b` is the big one: the move generator!

   This function should return the set of all legal moves that play
   n tiles from the rack r onto the board b, where t is the dictionary.

   Almost all the functions you wrote above are designed to be useful
   when writing the move generator, but it will take some non-trivial
   thinking to figure out how exactly.
 -}

moves :: Trie -> Int -> Rack -> Board -> Trie
moves trie n [] board = empty
moves trie n rack (Board board) =
  let
    allWordsOfLengthN = filterLength n trie
    verticalConstraints = getVerticalConstraints trie board n
    pattern = constraintToPattern verticalConstraints
    filteredByVerticals = filterPattern pattern allWordsOfLengthN
    playableWords = filterPlayables rack filteredByVerticals
  in
    trace ("All words of length " ++ show n ++ ": " ++ show allWordsOfLengthN) $
    trace ("Vertical constraints: " ++ show verticalConstraints) $
    trace ("Pattern: " ++ show pattern) $
    trace ("Filtered by verticals: " ++ show filteredByVerticals) $
    trace ("Playable words: " ++ show playableWords) $
    playableWords

-- Helper functions:

getVerticalConstraints :: Trie -> [((String, String), Maybe Char)] -> Int -> [[(Int, Constraint)]]
getVerticalConstraints trie board n =
  [getConstraintAtPosition trie board i | i <- [0..n-1]]

getConstraintAtPosition :: Trie -> [((String, String), Maybe Char)] -> Int -> [(Int, Constraint)]
getConstraintAtPosition trie board pos =
  case board !! pos of
    (("", ""), _) ->
      trace ("Position " ++ show pos ++ ": Wildcard")
      [(pos, Wildcard)]
    ((above, below), _) ->
      let sandwichable = sandwichableLetters trie above below
       in
        trace ("Position " ++ show pos ++ ": Sandwichable letters for " ++ show (above, below) ++ " -> " ++ show sandwichable)
        [(pos, Mem sandwichable)]

constraintToPattern :: [[(Int, Constraint)]] -> Pattern
constraintToPattern constraints =
  let
    -- Flatten the list of constraints into a single list
    flatConstraints = concat constraints
    -- Check if flatConstraints is empty and handle it
    pattern = if null flatConstraints
              then []
              else
                let
                  -- Find the maximum index to define the length of the pattern
                  maxIndex = maximum (map fst flatConstraints)
                in
                  -- Create the pattern
                  [fromMaybe Wildcard (lookup i flatConstraints)
                   | i <- [0..maxIndex]]
  in
    -- trace ("Combined constraints: " ++ show flatConstraints) $
    -- trace ("Pattern: " ++ show pattern) $
    pattern


type MoveGenerator = Trie -> Int -> Rack -> Board -> Trie

{- Write a function which, given a move generator `g` such as the
   one you wrote above, returns all playable moves of *any*
   length (according to `g`).

   You may find it useful to use the monoid instance you wrote above.

   To generate all moves according to *your* move generator, you would
   invoke `allMoves moves`.  This layer of indirection makes it
   possible for us to test `allMoves` independently of `moves` by
   plugging in our reference implementation.
 -}
allMoves :: MoveGenerator -> Trie -> Rack -> Board -> Trie
allMoves g t rack board = fromMonoid $ mconcat [TrieMonoid (g t len rack board) | len <- [0 .. rackSize]]
  where
    rackSize = length rack

{- TODO:
   If you've consistently used Tries instead of lists ---
   in particular, if you've refrained from using the
      fromList . doStuff . toList
   trick explained in the assignment spec,
   and if your assignment solution scores 18/20
   or more, then you are eligible for two bonus points
   on the final exam.

   Set trieOrDie to True to certify that you have used
   Tries consistently.
 -}
trieOrDie :: Bool
trieOrDie = True

{- Once you've implemented fromList,
   you can use this function to read a
   dictionary from a file of
   whitespace-separated words. Run the
   following in GHCi:

     myDict <- dictionaryFromFile "dictionary.txt"

   This will save the dictionary comprised
   of the words in dictionary.txt into the
   variable binding myDict.

   For now, don't worry too much about how
   this works!  It will become clearer later
   in the course.
 -}
dictionaryFromFile :: String -> IO Trie
dictionaryFromFile =
  readFile >=> return . fromList . words . map toLower

{- There's no need to understand the following code!
   What is does is prints out boards as ASCII art.

   Try running e.g.

      drawBoard at_board
 -}
drawBoard :: Board -> IO ()
drawBoard (Board []) = return ()
drawBoard (Board xs) =
  putStrLn $
    concat
      [ as',
        ">",
        intersperse '|' $ fromMaybe ' ' <$> cs,
        "\n",
        bs'
      ]
  where
    as = fst . fst <$> xs
    bs = snd . fst <$> xs
    cs = snd <$> xs
    alen = maximum $ length <$> as
    blen = maximum $ length <$> bs
    x = length xs
    as' =
      do
        n <- [0 .. alen - 1]
        let xs = getLetAb n as <$> [0 .. x - 1]
        '|' : intersperse '|' xs ++ "|\n"
    bs' =
      do
        n <- [0 .. blen - 1]
        let xs = getLetBe n bs <$> [0 .. x - 1]
        '|' : intersperse '|' xs ++ "|\n"
    getLetAb n ws x =
      let xs = ws !! x
       in if n < alen - length xs
            then
              ' '
            else xs !! (n + length xs - alen)
    getLetBe n ws x =
      let xs = ws !! x
       in if n < length xs
            then
              xs !! n
            else
              ' '

{- Here are the various example boards from the spec. -}

empty_board :: Board
empty_board =
  Board
    [ (("", ""), Nothing),
      (("", ""), Nothing),
      (("", ""), Nothing),
      (("", ""), Nothing),
      (("", ""), Nothing),
      (("", ""), Nothing),
      (("", ""), Nothing),
      (("", ""), Nothing)
    ]

aeintsr_rack :: Rack
aeintsr_rack =
  [ Letter 'a',
    Letter 'e',
    Letter 'i',
    Letter 'n',
    Letter 's',
    Letter 't',
    Letter 'r'
  ]

dog_board :: Board
dog_board =
  Board
    [ (("", ""), Nothing),
      (("", ""), Nothing),
      (("", ""), Nothing),
      (("", ""), Nothing),
      (("", ""), Nothing),
      (("d", "cent"), Nothing),
      (("o", ""), Nothing),
      (("g", ""), Nothing)
    ]

at_board :: Board
at_board =
  Board
    [ (("", ""), Nothing),
      (("", ""), Nothing),
      (("", ""), Just 'a'),
      (("", "alk"), Just 't'),
      (("", ""), Nothing),
      (("d", "cent"), Nothing),
      (("o", ""), Nothing),
      (("g", ""), Nothing)
    ]

-------------------
----- Helpers -----
-------------------

{- Compare two lists -}
compareLists :: Ord a => [a] -> [a] -> Bool
compareLists x y = sort x == sort y

{- Assert Trie == List -}
assertTrieList :: Trie -> [String] -> Bool
assertTrieList t l = compareLists (toList t) l

-----------------
----- moves -----
-----------------

{- Empty board, can play anything of correct length -}
testMoves1 :: Bool
testMoves1 = assertTrieList actual1 expected1 && assertTrieList actual2 expected2
    where
        trie = fromList ["ate", "eat", "age", "eta", "tea", "ae", "he", "at", "eaten", "teapot", "hate", "hi", "aw", "maw"]
        board = Board
            [(("", ""), Nothing),
             (("", ""), Nothing),
             (("", ""), Nothing),
             (("", ""), Nothing),
             (("", ""), Nothing)]
        r = [Letter 'a', Letter 't', Letter 'e']
        actual1 = moves trie 2 r board
        expected1 = ["ae", "at"]
        actual2 = moves trie 3 r board
        expected2 = ["ate", "eat", "eta", "tea"]

{- Tiles on main row, doesn't reach -}
testMoves2 :: Bool
testMoves2 = assertTrieList actual1 expected1 && assertTrieList actual2 expected2
    where
        trie = fromList ["ate", "eat", "age", "eta", "tea", "ae", "he", "at", "eaten", "teapot", "hate", "hi", "aw", "maw"]
        board = Board
            [(("", ""), Nothing),
             (("", ""), Nothing),
             (("", ""), Nothing),
             (("", ""), Nothing),
             (("", ""), Just 'b'),
             (("", ""), Just 'a'),
             (("", ""), Just 'd')]
        r = [Letter 'a', Letter 't', Letter 'e']
        actual1 = moves trie 2 r board
        expected1 = ["ae", "at"]
        actual2 = moves trie 3 r board
        expected2 = ["ate", "eat", "eta", "tea"]

{- Tiles on main row, start blocked -}
testMoves3 :: Bool
testMoves3 = assertTrieList actual expected
    where
        trie = fromList ["assert", "assert", "sert", "ass", "a", "as"]
        board = Board
            [(("", ""), Just 'a'),
             (("", ""), Just 's'),
             (("", ""), Nothing),
             (("", ""), Nothing),
             (("", ""), Nothing),
             (("", ""), Nothing),
             (("", ""), Nothing),
             (("", ""), Nothing),
             (("", ""), Nothing),
             (("", ""), Just 'a')]
        r = [Letter 'e', Letter 't', Letter 's', Letter 'r']
        actual = moves trie 4 r board
        expected = ["assert"]

{- Tiles on main row, end blocked -}
testMoves4 :: Bool
testMoves4 = assertTrieList actual expected
    where
        trie = fromList ["aid", "aide", "aids", "maid", "raid", "said", "idea"]
        board = Board
            [(("", ""), Nothing),
             (("", ""), Just 'a'),
             (("", ""), Just 'i'),
             (("", ""), Just 'd'),
             (("", ""), Nothing)]
        r = [Letter 'e', Letter 'r', Letter 's', Letter 'm']
        actual = moves trie 1 r board
        expected = ["maid", "raid", "said"]

{- Tiles on main row, start & middle blocked -}
testMoves5 :: Bool
testMoves5 = assertTrieList actual expected
    where
        trie = fromList ["assert", "assert", "sert", "ass", "a", "as"]
        board = Board
            [(("", ""), Just 'a'),
             (("", ""), Just 's'),
             (("", ""), Nothing),
             (("", ""), Just 'e'),
             (("", ""), Just 'r'),
             (("", ""), Nothing),
             (("", ""), Nothing)]
        r = [Letter 'e', Letter 't', Letter 's', Letter 'r']
        actual = moves trie 2 r board
        expected = ["assert"]

{- Tiles on main row, start & middle blocked -}
testMoves6 :: Bool
testMoves6 = assertTrieList actual expected
    where
        trie = fromList ["convincing", "rains", "constrains", "constraint", "constraints", "constrict"]
        board = Board
            [(("", ""), Just 'c'),
             (("", ""), Just 'o'),
             (("", ""), Just 'n'),
             (("", ""), Nothing),
             (("", ""), Nothing),
             (("", ""), Just 'r'),
             (("", ""), Just 'a'),
             (("", ""), Just 'i'),
             (("", ""), Just 'n'),
             (("", ""), Nothing),
             (("", ""), Nothing)]
        r = [Letter 't', Letter 't', Letter 's', Letter 's']
        actual = moves trie 3 r board
        expected = ["constraint", "constrains"]

{- Words above, none below -}
testMoves7 :: Bool
testMoves7 = assertTrieList actual expected
    where
        trie = fromList ["his", "him", "hit", "train", "mailt", "snail", "bell", "belt"]
        board = Board
            [(("hi", ""), Nothing),
             (("", ""), Nothing),
             (("", ""), Nothing),
             (("", ""), Nothing),
             (("bel", ""), Nothing),
             (("", ""), Nothing)]
        r = [Letter 'a', Letter 'i', Blank, Blank, Blank]
        actual = moves trie 5 r board
        expected = ["mailt", "snail"]

{- None above, words below -}
testMoves8 :: Bool
testMoves8 = assertTrieList actual expected
    where
        trie = fromList ["task", "bask", "mask", "madman", "made", "bade", "lade", "tubble"]
        board = Board
            [(("", "ask"), Nothing),
             (("", ""), Nothing),
             (("", ""), Nothing),
             (("", "ade"), Nothing),
             (("", ""), Nothing),
             (("", ""), Nothing)]
        r = [Blank, Blank, Blank, Blank, Blank, Blank]
        actual = moves trie 6 r board
        expected = ["madman", "tubble"]

{- Words above, words below -}
testMoves9 :: Bool
testMoves9 = assertTrieList actual expected
    where
        trie = fromList ["task", "bask", "mask", "madman", "made", "bade", "lade", "tubble", "sad", "sat", "tub"]
        board = Board
            [(("", "ask"), Nothing),
             (("", ""), Nothing),
             (("sa", ""), Nothing),
             (("", "ade"), Nothing),
             (("", ""), Nothing),
             (("", ""), Nothing)]
        r = [Blank, Blank, Blank, Blank, Blank, Blank]
        actual = moves trie 6 r board
        expected = ["madman"]

{- List of tests for `moves` -}
listTestsMoves :: [Bool]
listTestsMoves = [testMoves1, testMoves2, testMoves3, testMoves4, testMoves5, testMoves6, testMoves7, testMoves8, testMoves9]

{- Run all tests for `moves` -}
allTestsMoves :: Bool
allTestsMoves = all (== True) listTestsMoves


--------------------
----- allMoves -----
--------------------

{- Tiles on main row, start & middle blocked -}
testAllMoves1 :: Bool
testAllMoves1 = assertTrieList actual expected
    where
        trie = fromList ["convincing", "rains", "constrains", "constraint", "constraints", "constrict"]
        board = Board
            [(("", ""), Just 'c'),
             (("", ""), Just 'o'),
             (("", ""), Just 'n'),
             (("", ""), Nothing),
             (("", ""), Nothing),
             (("", ""), Just 'r'),
             (("", ""), Just 'a'),
             (("", ""), Just 'i'),
             (("", ""), Just 'n'),
             (("", ""), Nothing),
             (("", ""), Nothing)]
        r = [Letter 't', Letter 't', Letter 's', Letter 's']
        actual = allMoves moves trie r board
        expected = ["constraint", "constrains", "constraints"]

{- Words above, words below -}
testAllMoves2 :: Bool
testAllMoves2 = assertTrieList actual expected
    where
        trie = fromList ["task", "bask", "mask", "madman", "made", "bade", "lade", "tubble", "sad", "sat", "tub", "ta", "tad", "mad"]
        board = Board
            [(("", "ask"), Nothing),
             (("", ""), Nothing),
             (("sa", ""), Nothing),
             (("", "ade"), Nothing),
             (("", ""), Nothing),
             (("", ""), Nothing)]
        r = [Blank, Blank, Blank, Blank, Blank, Blank]
        actual = allMoves moves trie r board
        expected = ["madman", "ta", "tad", "mad"]

{- Doesn't pick up 1 letter words -}
testAllMoves3 :: Bool
testAllMoves3 = assertTrieList actual expected
    where
        trie = fromList ["a", "at", "ask"]
        board = Board
            [(("", ""), Nothing),
             (("", ""), Nothing),
             (("", ""), Nothing)]
        r = [Blank, Blank, Blank]
        actual = allMoves moves trie r board
        expected = ["at", "ask"]

{- Must have played at least 1 tile -}
testAllMoves4 :: Bool
testAllMoves4 = assertTrieList actual expected
    where
        trie = fromList ["a", "at", "ate", "attend"]
        board = Board
            [(("", ""), Just 'a'),
             (("", ""), Just 't'),
             (("", ""), Nothing),
             (("", ""), Nothing),
             (("", ""), Nothing),
             (("", ""), Nothing)]
        r = [Blank, Blank, Blank, Blank, Blank, Blank]
        actual = allMoves moves trie r board
        expected = ["ate", "attend"]

{- List of tests for `allMoves` -}
listTestsAllMoves :: [Bool]
listTestsAllMoves = [testAllMoves1, testAllMoves2, testAllMoves3, testAllMoves4]

{- Run all tests for `allMoves` -}
allTestsAllMoves :: Bool
allTestsAllMoves = all (== True) listTestsAllMoves
