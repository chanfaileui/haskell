module MoveGenerator where

{- Feel free to import more stuff here, as long as it's all from
   `base`. In other words, importing it should not require tinkering
   with the stack.yaml or .cabal files.
 -}
import Data.Maybe(fromMaybe)
import Data.List(intersperse,sort,nub)
import Data.Char(toLower)
import Control.Monad((>=>))
import Test.QuickCheck

-- ADDED MYSELF
import Data.Maybe
import Data.List (unionBy)
import Data.List (permutations)


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

   Trie False [('h', Trie False [('e', Trie True [('l', Trie False [('l', Trie True [('o', Trie True []) ] )] )] ), ('i', Trie True [])])]
 -}
data Trie = Trie Bool [(Char,Trie)] deriving (Eq,Show)

{- `empty` represents an empty dictionary. -}
empty :: Trie
empty = Trie False []

{- `single xs` represents a dictionary consisting of only `xs`. -}
single :: String -> Trie
single []     = Trie True []
single (x:xs) = Trie False [(x,single xs)]

{- `insert t xs` inserts the word xs into the dictionary t. -}
insert :: String -> Trie -> Trie
insert [] (Trie _ ts)     = Trie True ts
insert (x:xs) (Trie b ts) =
  case span ((<x) . fst) ts of
    (ts1,[]) -> Trie b $ ts1 ++ [(x,single xs)]
    (ts1,(y,t):ts2)
      | x == y    -> Trie b $ ts1 ++ (x,insert xs t):ts2
      | otherwise -> Trie b $ ts1 ++ (x,single xs):(y,t):ts2

{- `toList t` gives the list of all words in t.
   If `t` is well-formed, they will come out in alphabetical order.
 -}
toList :: Trie -> [String]
toList (Trie b ts) =
  first ++ rest where
  first | b         = [""]
        | otherwise = []
  rest = concatMap (\(x,t) -> map (x:) $ toList t) ts

{- `fromList ws` should return a dictionary containing
   exactly the words in ws.
 -}
-- helper function
insertAll :: [String] -> Trie -> Trie
insertAll [] trie = trie
insertAll (x:xs) trie = insertAll xs (insert x trie)

fromList :: [String] -> Trie
fromList wordList = insertAll wordList empty

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

-- isSorted goes through and check whether the chars in the same level are in alphabetical order, and each char has at most one entry
wellFormedRecurse :: [(Char, Trie)] -> Bool
wellFormedRecurse [] = True
wellFormedRecurse [(c, ts)] = wellFormed ts
wellFormedRecurse ((char1, tr1):(char2, tr2):rest) = wellFormed tr1 && wellFormedRecurse ((char2, tr2):rest)

wellFormed :: Trie -> Bool
wellFormed (Trie b []) = True
wellFormed (Trie b [_]) = True
wellFormed (Trie b ((char1, tr1):(char2, tr2):rest)) = char1 < char2  && wellFormedRecurse ((char1, tr1):(char2, tr2):rest)

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

isMinimal :: Trie -> Bool
isMinimal (Trie b ts)=
  first && rest where 
  first | (Trie b ts) == empty = False
        | otherwise = True
  rest = all (\(_,t) -> isMinimal t) ts

minimal :: Trie -> Bool
minimal (Trie b ts)=
  first && rest where 
  first = True
  rest = all (\(_,t) -> isMinimal t) ts

{- Write a `prune t` which returns a minimal
   trie representing the same dictionary as t.
 -}
-- Prune function with help from Thursday week 5 Lecture
prune :: Trie -> Trie
prune (Trie b ts)
  = Trie b $ mapMaybe (\(c', t) ->
      if True
    then (if prune t == Trie False []
      then Nothing
      else Just (c', prune t))
    else Just (c', t)
    ) ts

{- Here's a generator and associated
   Arbitrary instance for use with QuickCheck.
   This should only generate well-formed Tries,
   but is not guaranteed to generate minimal Tries.

   It's *not* necessary to fully understand what's
   going on here.
 -}
genTrie :: Int -> Gen Trie
genTrie 0 = pure $ Trie True []
genTrie n =
  Trie <$> arbitrary <*> (genKeys >>= genSubtries) where
  genKeys :: Gen [Char]
  genKeys = sort . nub <$> (resize 5 . listOf $ elements ['a'..'z'])
  genSubtries :: [Char] -> Gen [(Char,Trie)]
  genSubtries cs =
      zip cs <$> vectorOf (length cs) (genTrie . max 0 $ n-1-length cs)

instance Arbitrary Trie where
  arbitrary = sized $ genTrie . min 15
  shrink (Trie b ts) =
    (Trie b <$> shrinkList (const []) ts) ++
    (Trie b <$> map shrink ts)

{- `check t xs` should return True if `xs` is
   in the dictionary `t`, and False otherwise.
 -}
--checkToList :: Trie -> String -> Bool
--checkToList (Trie b ts) word = elem word (toList (Trie b ts))

-- Prune function with help from Thursday week 5 Lecture
check :: Trie -> String -> Bool
check (Trie b ts) "" = b
check (Trie b ts) (c:cs)
  = any (\(c', t) ->
      if c' == c
    then (if check t cs == False
      then False
      else True)
    else False
    ) ts

{- The union of two dictionaries t,t' should contain
   all words that occur in either t or t'.
 -}

union :: Trie -> Trie -> Trie
union (Trie b ts) (Trie b' ts') = 
    Trie (b || b') (unionTrie ts ts') where
        unionTrie :: [(Char,Trie)] -> [(Char,Trie)] -> [(Char,Trie)]
        unionTrie [] ts2 = ts2
        unionTrie ts1 [] = ts1
        unionTrie ((x1,t1):ts1) ((x2,t2):ts2)
            | x1 < x2 = (x1,t1):(unionTrie ts1 ((x2,t2):ts2))
            | x1 > x2 = (x2,t2):(unionTrie ((x1,t1):ts1) ts2)
            | otherwise = (x1,union t1 t2):(unionTrie ts1 ts2)
{- The intersection of two dictionaries t,t' should contain
   all words that occur in *both* t and t'.
 -}

-- With help from weekly quiz question
intersection :: Trie -> Trie -> Trie
intersection (Trie b ts) (Trie b' ts') =
  Trie (b && b') (concat (map intersectTrie ts)) where
  intersectTrie :: (Char,Trie) -> [(Char,Trie)]
  intersectTrie (x,t) =
    case lookup x ts' of
      Nothing -> []
      Just t' -> dropEmpty (x, intersection t t')
  dropEmpty :: (Char,Trie) -> [(Char,Trie)]
  dropEmpty (_,Trie False []) = []
  dropEmpty t = [t]
{- One of the above (union or intersection) forms
   a monoid with `empty` as identity element.
   Use the right one to define the following monoid
   instance:
 -}
newtype TrieMonoid = TrieMonoid {fromMonoid :: Trie} deriving (Eq,Show)

instance Semigroup TrieMonoid where
  (<>) (TrieMonoid t1) (TrieMonoid t2) = TrieMonoid (union t1 t2)

instance Monoid TrieMonoid where
  mappend = (<>)
  mempty =  TrieMonoid empty

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

   - If `elem x xs`, then `pick x xs = (True,ys)` for some `ys`
                     such that `xs` is a permutation of `x:ys`.
   - If `not(elem x xs)`, then `pick x xs = (False,ys)`, for some `ys`
                          such that `xs` is a permutation of `ys`.

   This utility function is useful for pulling out specific tiles from a rack.
 -}
pick :: Eq a => [a] -> a -> (Bool, [a])
pick [] _ = (False, [])
pick (x:xs) y
  | x == y = (True, xs)
  | otherwise = case pick xs y of
                  (found, rest) -> (found, x : rest)

{- `sandwichableLetters t xs ys` should return a list
   containing all the characters x such that
   `xs++x:ys` is a word in `t`.

   This will be handy for collecting constraints imposed by words
   formed vertically.
 -}
sandwichableLetters :: Trie -> String -> String -> [Char]
sandwichableLetters (Trie b ts) xs ys =
  [x | x <- nub(concatMap id (toList(Trie b ts))), check (Trie b ts) (xs ++ [x] ++ ys)]

{- A constraint represents a predicate on characters.
   A character c is said to *match* a constraint
   according to the following clauses:
  
   - Any character c matches `Wildcard`.
   - A character c matches `Mem cs`, if c occurs in cs.
 -}
data Constraint = Wildcard | Mem String deriving (Show,Eq)

{- QuickCheck generator for constraints.
   Not necessary to follow all the details. -}
instance Arbitrary Constraint where
  arbitrary = oneof [pure Wildcard,
                     Mem . sort . nub <$> listOf(elements ['a'..'z'])]
  shrink Wildcard = []
  shrink (Mem xs) = Mem <$> shrinkList (:[]) xs

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
filterLength n ts = fromList (filter (\x -> length x == n) (toList ts))


{- `filterPattern cs t` should return
   a dictionary containing all words in t
   that matches the pattern cs.
 -}
patternMatch :: String -> Pattern -> Bool
patternMatch xs cs = (length xs == length cs) && all (\(x,c) -> isMatch x c) (zip xs cs)
    where isMatch :: Char -> Constraint -> Bool
          isMatch _ (Wildcard) = True
          isMatch char (Mem s) 
            | elem char s = True
            | otherwise = False

filterPattern :: Pattern -> Trie -> Trie
filterPattern p (Trie b ts) = fromList(filter (\x -> patternMatch x p) (toList (Trie b ts)))

{- A Tile is either a letter tile, or a blank tile.
   Blank tiles are the most OP thing in the game:
   they are wildcards that can be played as any
   letter.

   A Rack is just a list of Tiles..
 -}
data Tile = Letter Char | Blank deriving (Eq,Show)
type Rack = [Tile]

{- QuickCheck generator for tiles.
   Very biased towards Blank tiles. -}
instance Arbitrary Tile where
  arbitrary = oneof [pure Blank,
                     Letter <$> elements ['a'..'z']]

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

filterPlayables:: Rack -> Trie -> Trie
filterPlayables rack trie = fromList(filter (canFormWord rack) (toList trie))

canFormWord :: Rack -> String -> Bool
canFormWord rack word = any (canFormPermutation word) (permutations rack)

canFormPermutation :: String -> Rack -> Bool
canFormPermutation [] _ = True
canFormPermutation _ [] = False
canFormPermutation (c:cs) rack = case findTile c rack of 
  Just remainingRack -> canFormPermutation cs remainingRack
  Nothing -> False

findTile :: Char -> Rack -> Maybe Rack
findTile _ [] = Nothing
findTile c (tile:rack) = case tile of
  Letter letter -> if c == letter then Just rack else findTile c rack
  Blank -> Just rack


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
newtype Board = Board [((String,String),Maybe Char)] deriving (Eq,Show)

{- `moves t n r b` is the big one: the move generator!

   This function should return the set of all legal moves that play
   n tiles from the rack r onto the board b, where t is the dictionary.

   Almost all the functions you wrote above are designed to be useful
   when writing the move generator, but it will take some non-trivial
   thinking to figure out how exactly.
 -}
--moves :: Trie -> Int -> Rack -> Board -> Trie
moves :: Trie -> Int -> Rack -> Board -> Trie
moves trie n [] board = empty
moves trie n rack board =
  fromList (removeSingleCharStrings (iterateList (getCompleteSubPatternList (createPattern trie board (rackToString rack)) (countBoard board) n) (filterPlayables (appendBlanks rack (countChars board)) trie)))

removeSingleCharStrings :: [String] -> [String]
removeSingleCharStrings xs = filter (\str -> length str /= 1) xs

iterateList :: [Pattern] -> Trie -> [String]
iterateList [] _ = []
iterateList (c : cs) trie = toList (filterPattern c trie) ++ iterateList cs trie

countChars :: Board -> Int
countChars (Board cells) = length [char | (_, Just char) <- cells]

appendBlanks :: Rack -> Int -> Rack
appendBlanks rack n = rack ++ replicate n Blank

rackToString :: Rack -> String
rackToString letters = concat (map (\(Letter c) -> [c]) letters)

createPattern :: Trie -> Board -> String -> Pattern
createPattern trie (Board cells) avaliableChars = concatMap processCell cells
  where
    processCell :: ((String, String), Maybe Char) -> Pattern
    processCell ((str1, str2), tile) =
      let result = sandwichableLetters trie str1 str2
       in case (result, tile) of
            ("", Nothing) ->
              if null str1 && null str2
                then [Mem avaliableChars]
                else [Mem ""]
            (chars, Nothing) ->
              if length chars == 1 && null str1 && null str2
                then [Wildcard]
                else [Mem chars]
            (chars, Just x) -> [Mem [x]]

countBoard :: Board -> [Int]
countBoard (Board cells) = map numberTiles cells
  where
    numberTiles :: ((String, String), Maybe Char) -> Int
    numberTiles (_, Nothing) = 1
    numberTiles (_, Just _) = 0

-- FUNCTIONS FOR GETTING SUB LISTS OF PATTERN
getCompleteSubPatternList :: Pattern -> [Int] -> Int -> [Pattern]
getCompleteSubPatternList pattern intList 0 = []
getCompleteSubPatternList pattern intList n
  | countOnes intList < n = []
  | otherwise = mergeLists (removeLastN n (createSubPatterns intList pattern n)) (removeLastN n (revertList (createSubPatterns (reverse intList) (reverse pattern) n)))

countOnes :: [Int] -> Int
countOnes xs = length (filter (== 1) xs)

createSubPatterns :: [Int] -> Pattern -> Int -> [Pattern]
createSubPatterns [] _ _ = [[]]
createSubPatterns (x : xs) (c : cs) n = subPatterns (x : xs) (c : cs) n 0 ++ createSubPatterns xs cs n
  where
    subPatterns :: [Int] -> Pattern -> Int -> Int -> [Pattern]
    subPatterns [] _ _ _ = [[]]
    subPatterns (x : xs) (c : cs) n count
      | count + x <= n = [c : subPattern | subPattern <- subPatterns xs cs n (count + x)]
      | otherwise = [[]]

revertList :: [Pattern] -> [Pattern]
revertList = map reverse

removeLastN :: Int -> [Pattern] -> [Pattern]
removeLastN n xs = take (length xs - n) xs

mergeLists :: [Pattern] -> [Pattern] -> [Pattern]
mergeLists [] ys = []
mergeLists (x : xs) ys
  | x `elem` ys = x : mergeLists xs ys
  | otherwise = mergeLists xs ys

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
trieOrDie = False

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
drawBoard :: Board -> IO()
drawBoard (Board []) = return ()
drawBoard (Board xs) =
  putStrLn $ concat
    [as',
     ">",
     intersperse '|' $ fromMaybe ' ' <$> cs,
     "\n",
     bs'] where
  as = fst . fst <$> xs
  bs = snd . fst <$> xs
  cs = snd <$> xs
  alen  = maximum $ length <$> as
  blen  = maximum $ length <$> bs
  x = length xs
  as' =
    do
      n <- [0..alen-1]
      let xs = getLetAb n as <$> [0..x-1]
      '|':intersperse '|' xs++"|\n"
  bs' =
    do
      n <- [0..blen-1]
      let xs = getLetBe n bs <$> [0..x-1]
      '|':intersperse '|' xs++"|\n"
  getLetAb n ws x =
    let xs = ws !! x in
      if n < alen - length xs then
        ' '
      else xs !! (n + length xs - alen)
  getLetBe n ws x =
    let xs = ws !! x in
      if n < length xs then
        xs !! n
      else
        ' '

{- Here are the various example boards from the spec. -}

empty_board :: Board
empty_board =
  Board
    [(("",""),Nothing),
     (("",""),Nothing),
     (("",""),Nothing),
     (("",""),Nothing),
     (("",""),Nothing),
     (("",""),Nothing),
     (("",""),Nothing),
     (("",""),Nothing)
    ]

aeintsr_rack :: Rack
aeintsr_rack =
  [Letter 'a',
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
    [(("",""),Nothing),
     (("",""),Nothing),
     (("",""),Nothing),
     (("",""),Nothing),
     (("",""),Nothing),
     (("d","cent"),Nothing),
     (("o",""),Nothing),
     (("g",""),Nothing)
    ]

at_board :: Board
at_board =
  Board
    [(("",""),Nothing),
     (("",""),Nothing),
     (("",""),Just 'a'),
     (("","alk"),Just 't'),
     (("",""),Nothing),
     (("d","cent"),Nothing),
     (("o",""),Nothing),
     (("g",""),Nothing)
    ]


--MY TESTS
test_board :: Board
test_board =
  Board
    [(("",""),Nothing),
     (("",""),Nothing),
     (("",""),Nothing),
     (("","alk"),Nothing),
     (("",""),Nothing),
     (("d","cent"),Nothing),
     (("o",""),Nothing),
     (("g",""),Nothing)
    ]
daniel_board :: Board
daniel_board =
  Board
    [(("",""),Just 'a'),
      (("",""),Just 'a'),
      (("",""),Just 'a'),
      (("",""),Just 'a'),
      (("",""),Just 'a'),
      (("",""),Just 'a'),
      (("",""),Just 'a'),
      (("",""),Nothing)
    ]
shifty_board :: Board
shifty_board =
  Board
    [(("d","cent"),Nothing),
      (("",""),Nothing),
      (("",""),Nothing),
      (("",""),Nothing),
      (("",""),Nothing),
      (("",""),Nothing),
      (("",""),Nothing),
      (("",""),Nothing)
    ]
testFromList :: [String] -> Bool
testFromList s = (sort (toList $ fromList s) == sort (nub s)) && wellFormed (fromList s)

testPrune :: Trie -> Bool
testPrune t
    | minimal t = (prune t == t)
    | otherwise = minimal (prune t)

testCheck :: Trie -> String -> Bool
testCheck t xs = check t xs == elem xs (toList t)

common :: [String] -> [String] -> [String]
common [] _ = []
common (x:xs) ys
    | elem x ys = x : common xs ys
    | otherwise = common xs ys

testIntersection :: Trie -> Trie -> Bool
testIntersection t t' = intersection t t' == fromList (common (toList t) (toList t'))


--Tests from forum
movesUnitTests :: Bool
movesUnitTests =
  null (toList $ moves (fromList ["nastier", "decent"]) 7 aeintsr_rack dog_board) &&
  null (toList $ moves (fromList ["nastier", "or"]) 7 aeintsr_rack dog_board) &&
  toList (moves (fromList ["decent", "or", "nastier"]) 7 aeintsr_rack dog_board) == ["nastier"] &&
  toList (moves (fromList ["state", "stat", "st"]) 2 aeintsr_rack at_board) == ["stat"] &&
  toList (moves (fromList ["state", "stat", "st"]) 3 aeintsr_rack at_board) == ["state"] &&
  toList (moves (fromList ["stater", "statei", "dicent"]) 4 aeintsr_rack at_board) == ["statei"] &&
  toList (moves (fromList ["stateeee", "stateira", "dicent", "decent", "oe", "ge", "dicent", "or", "ga"]) 6 aeintsr_rack at_board) == ["stateira"] &&
  sort (toList (moves (fromList ["aaaaaaae", "aaaaaaam", "aaaaaaat", "aaaaaaar", "aaaaaace"]) 1 aeintsr_rack daniel_board)) == ["aaaaaaae", "aaaaaaar", "aaaaaaat"] &&
  null (toList (moves (fromList ["aaaaaaae", "aaaaaaam", "aaaaaaat", "aaaaaaar", "aaaaaace"]) 8 aeintsr_rack daniel_board)) &&
  null (toList $ moves (fromList ["a", "e", "i", "n", "t", "s", "r"]) 1 aeintsr_rack empty_board) &&
  null (toList $ moves (fromList ["ae", "ee", "ie", "ne", "te", "se", "re"]) 1 aeintsr_rack empty_board) &&
  sort (toList $ moves (fromList ["ae", "ee", "ie", "ne", "te", "se", "re"]) 2 aeintsr_rack empty_board) == ["ae","ie","ne","re","se","te"] &&
  null (toList $ moves (fromList ["e", "decent"]) 1 aeintsr_rack shifty_board) &&
  null (toList $ moves (fromList ["e", "decent"]) 5 aeintsr_rack shifty_board) &&
  null (toList (moves (fromList ["aaaaaaa", "aaaaaa", "aaaaa", "aaaa", "aaa", "aa", "a", ""]) 0 aeintsr_rack daniel_board))


allMovesUnitTests :: Bool
allMovesUnitTests = 
  toList (allMoves (moves) a_dict a_rack empty_board) == tail (toList a_dict) && 
  toList (allMoves (moves) a_dict a_rack mini_empty_board) == ["aa", "aaa", "aaaa"] &&
  toList (allMoves (moves) ab_dict ablank_rack mini_empty_board) == ["aa", "aaa", "aaaa", "abab", "ba", "bb"]

a_dict :: Trie
a_dict = fromList ["a", "aa", "aaa", "aaaa", "aaaaa", "aaaaaa", "aaaaaaaa", "aaaaaaaa"]
a_rack :: Rack
a_rack = [Letter 'a', Letter 'a', Letter 'a', Letter 'a', Letter 'a', Letter 'a',Letter 'a', Letter 'a', Letter 'a'];
ab_dict :: Trie
ab_dict = fromList ["a", "aa", "ba", "aaa", "aaaa", "bbba", "abbb", "abab", "bb", "bbb"]
ablank_rack :: Rack 
ablank_rack = [Letter 'a', Letter 'a', Letter 'a', Letter 'a', Letter 'a', Blank, Blank];
mini_empty_board :: Board
mini_empty_board = 
  Board
    [(("",""),Nothing),
    (("",""),Nothing),
    (("",""),Nothing),
    (("",""),Nothing)
    ]


allDanielTests :: Bool
allDanielTests = testMove1 && testMove2 && testMove3 && testMove4 && testMove5 && testMove6 && testMove7 && testMove8 && testMove9 && testMove10 && (all (id) testMoveDanielBoards) && (all (id) testMoveShiftyBoard)


-- test suite by Daniel Chen on the forums
-- break these up to individual calls
testMove1 :: Bool
testMove1 = null (toList $ moves (fromList ["nastier", "decent"]) 7 aeintsr_rack dog_board)

testMove2 :: Bool
testMove2 = null (toList $ moves (fromList ["nastier", "or"]) 7 aeintsr_rack dog_board)

testMove3 :: Bool
testMove3 = toList (moves (fromList ["decent", "or", "nastier"]) 7 aeintsr_rack dog_board) == ["nastier"]

testMove4 :: Bool
testMove4 = toList (moves (fromList ["state", "stat", "st"]) 2 aeintsr_rack at_board) == ["stat"]

testMove5 :: Bool
testMove5 = toList (moves (fromList ["state", "stat", "st"]) 3 aeintsr_rack at_board) == ["state"]

testMove6 :: Bool
testMove6 = toList (moves (fromList ["stater", "statei", "dicent"]) 4 aeintsr_rack at_board) == ["statei"]

testMove7 :: Bool
testMove7 = toList (moves (fromList ["stateeee", "stateira", "dicent", "decent", "oe", "ge", "dicent", "or", "ga"]) 6 aeintsr_rack at_board) == ["stateira"]

testMoveDanielBoards :: [Bool]
testMoveDanielBoards = 
  [sort (toList (moves (fromList ["aaaaaaae", "aaaaaaam", "aaaaaaat", "aaaaaaar", "aaaaaace"]) 1 aeintsr_rack daniel_board)) == ["aaaaaaae", "aaaaaaar", "aaaaaaat"], 
  null (toList (moves (fromList ["aaaaaaae", "aaaaaaam", "aaaaaaat", "aaaaaaar", "aaaaaace"]) 8 aeintsr_rack daniel_board)), 
  null (toList (moves (fromList ["aaaaaaa", "aaaaaa", "aaaaa", "aaaa", "aaa", "aa", "a", ""]) 0 aeintsr_rack daniel_board))]
    where
  daniel_board =
    Board
      [(("",""),Just 'a'),
       (("",""),Just 'a'),
       (("",""),Just 'a'),
       (("",""),Just 'a'),
       (("",""),Just 'a'),
       (("",""),Just 'a'),
       (("",""),Just 'a'),
       (("",""),Nothing)
      ]

testMove8 :: Bool
testMove8 = null (toList $ moves (fromList ["a", "e", "i", "n", "t", "s", "r"]) 1 aeintsr_rack empty_board)

testMove9 :: Bool
testMove9 = null (toList $ moves (fromList ["ae", "ee", "ie", "ne", "te", "se", "re"]) 1 aeintsr_rack empty_board)

testMove10 :: Bool
testMove10 = sort (toList $ moves (fromList ["ae", "ee", "ie", "ne", "te", "se", "re"]) 2 aeintsr_rack empty_board) == ["ae","ie","ne","re","se","te"]

testMoveShiftyBoard :: [Bool]
testMoveShiftyBoard = 
  [null (toList $ moves (fromList ["e", "decent"]) 1 aeintsr_rack shifty_board),
  null (toList $ moves (fromList ["e", "decent"]) 5 aeintsr_rack shifty_board)]
    where
  shifty_board =
    Board
      [(("d","cent"),Nothing),
       (("",""),Nothing),
       (("",""),Nothing),
       (("",""),Nothing),
       (("",""),Nothing),
       (("",""),Nothing),
       (("",""),Nothing),
       (("",""),Nothing)
      ]