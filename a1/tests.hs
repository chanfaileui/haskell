-------------------
----- Helpers -----
-------------------

{- Compare two lists -}
compareLists :: Ord a => [a] -> [a] -> Bool
compareLists x y = sort x == sort y

{- Assert Trie == List -}
assertTrieList :: Trie -> [String] -> Bool
assertTrieList t l = compareLists (toList t) l

---------------------
----- All Tests -----
---------------------

listAllTests :: [Bool]
listAllTests = [allTestsFromList,
    allTestsWellFormed,
    allTestsMinimal,
    allTestsPrune,
    allTestsCheck,
    allTestsUnion,
    allTestsIntersection,
    allTestsPick,
    allTestsFilterLength,
    allTestsFilterPattern,
    allTestsFilterPlayables,
    allTestsMoves,
    allTestsAllMoves]

runAllTests :: Bool
runAllTests = all (== True) listAllTests


--------------------
----- fromList -----
--------------------

{- Empty list -}
testFromList1 :: Bool
testFromList1 = fromList list == Trie False []
    where
        list = []

{- Empty string -}
testFromList2 :: Bool
testFromList2 = fromList list == Trie True []
    where
        list = [""]

{- Single string -}
testFromList3 :: Bool
testFromList3 = fromList list == Trie False [('h', Trie False [('e', Trie False [('l', Trie False [('l', Trie False [('o', Trie True [])])])])])]
    where
        list = ["hello"]

{- Multiple strings -}
testFromList4 :: Bool
testFromList4 = fromList list == Trie False [('h', Trie False [('e', Trie False [('l', Trie False [('l', Trie False [('o', Trie True [])])])]), ('i', Trie True [])])]
    where
        list = ["hello", "hi"]

{- Complex strings -}
testFromList5 :: Bool
testFromList5 = assertTrieList (fromList list) list
    where
        list = ["a", "armadillo", "apple", "cat", "car", "cattle", "sat", "set", "met", "more", "hello", "moreover"]

{- List of tests for `fromList` -}
listTestsFromList :: [Bool]
listTestsFromList = [testFromList1, testFromList2, testFromList3, testFromList4, testFromList5]

{- Run all tests for `fromList` -}
allTestsFromList :: Bool
allTestsFromList = all (== True) listTestsFromList


----------------------
----- wellFormed -----
----------------------

{- Empty Trie -}
testWellFormed1 :: Bool
testWellFormed1 = wellFormed trie == True
    where
        trie = Trie False []

{- Empty string -}
testWellFormed2 :: Bool
testWellFormed2 = wellFormed trie == True
    where
        trie = Trie True []

{- Single string -}
testWellFormed3 :: Bool
testWellFormed3 = wellFormed trie == True
    where
        trie = Trie False [('h', Trie False [('e', Trie False [('l', Trie False [('l', Trie False [('o', Trie True [])])])])])]

{- Char sorted correct -}
testWellFormed4 :: Bool
testWellFormed4 = wellFormed trie ==  True
    where
        trie = Trie False [('a', Trie True []), ('h', Trie False [('e', Trie True []), ('i', Trie True [])])]

{- Char sorted wrong -}
testWellFormed5 :: Bool
testWellFormed5 = wellFormed trie1 == False && wellFormed trie2 == False
    where
        trie1 = Trie False [('h', Trie False [('e', Trie True []), ('i', Trie True [])]), ('a', Trie True [])]
        trie2 = Trie False [('a', Trie True []), ('h', Trie False [('i', Trie True []), ('e', Trie True [])])]

{- Duplicate char -}
testWellFormed6 :: Bool
testWellFormed6 = wellFormed trie == False
    where
        trie = Trie False [('a', Trie True []), ('h', Trie False [('e', Trie True []), ('e', Trie True []), ('i', Trie True [])])]

{- List of tests for `wellFormed` -}
listTestsWellFormed :: [Bool]
listTestsWellFormed = [testWellFormed1, testWellFormed2, testWellFormed3, testWellFormed4, testWellFormed5, testWellFormed6]

{- Run all tests for `wellFormed` -}
allTestsWellFormed :: Bool
allTestsWellFormed = all (== True) listTestsWellFormed


-------------------
----- minimal -----
-------------------

{- Empty Trie -}
testMinimal1 :: Bool
testMinimal1 = minimal trie == True
    where
        trie = Trie False []

{- Empty string -}
testMinimal2 :: Bool
testMinimal2 = minimal trie == True
    where
        trie = Trie True []

{- Single character valid -}
testMinimal3 :: Bool
testMinimal3 = minimal trie == True
    where
        trie = Trie False [('a', Trie True [])]

{- Complex Trie valid -}
testMinimal4 :: Bool
testMinimal4 = minimal trie == True
    where
        trie = Trie False [('h', Trie False [('e', Trie True [('l', Trie False [('l', Trie True [])])]), ('i', Trie True [('t', Trie True []), ('m', Trie True [])])])]

{- Whole Trie dead -}
testMinimal5 :: Bool
testMinimal5 = minimal trie == False
    where
        trie = Trie False [('a', Trie False [])]

{- Leaf dead -}
testMinimal6 :: Bool
testMinimal6 = minimal trie == False
    where
        trie = Trie False [('a', Trie True [('p', Trie False [])])]

{- Leaf at different level dead -}
testMinimal7 :: Bool
testMinimal7 = minimal trie == False
    where
        trie = Trie False [('a', Trie True [('p', Trie False [('t', Trie True [])])]), ('h', Trie False [('e', Trie True []), ('a', Trie False [])])]

{- List of tests for `minimal` -}
listTestsMinimal :: [Bool]
listTestsMinimal = [testMinimal1, testMinimal2, testMinimal3, testMinimal4, testMinimal5, testMinimal6, testMinimal7]

{- Run all tests for `minimal` -}
allTestsMinimal :: Bool
allTestsMinimal = all (== True) listTestsMinimal


-----------------
----- prune -----
-----------------

{- Empty Trie -}
testPrune1 :: Bool
testPrune1 = minimal pruned == True && compareLists (toList pruned) (toList trie)
    where
        pruned = prune trie
        trie = Trie False []

{- Empty string -}
testPrune2 :: Bool
testPrune2 = minimal pruned == True && compareLists (toList pruned) (toList trie)
    where
        pruned = prune trie
        trie = Trie True []

{- Single character valid -}
testPrune3 :: Bool
testPrune3 = minimal pruned == True && compareLists (toList pruned) (toList trie)
    where
        pruned = prune trie
        trie = Trie False [('a', Trie True [])]

{- Complex Trie valid -}
testPrune4 :: Bool
testPrune4 = minimal pruned == True && compareLists (toList pruned) (toList trie)
    where
        pruned = prune trie
        trie = Trie False [('h', Trie False [('e', Trie True [('l', Trie False [('l', Trie True [])])]), ('i', Trie True [('t', Trie True []), ('m', Trie True [])])])]

{- Whole Trie dead -}
testPrune5 :: Bool
testPrune5 = minimal pruned == True && compareLists (toList pruned) (toList trie)
    where
        pruned = prune trie
        trie = Trie False [('a', Trie False [])]

{- Leaf dead -}
testPrune6 :: Bool
testPrune6 = minimal pruned == True && compareLists (toList pruned) (toList trie)
    where
        pruned = prune trie
        trie = Trie False [('a', Trie True [('p', Trie False [])])]

{- Leaf at different level dead -}
testPrune7 :: Bool
testPrune7 = minimal pruned == True && compareLists (toList pruned) (toList trie)
    where
        pruned = prune trie
        trie = Trie False [('a', Trie True [('p', Trie False [('t', Trie True [])])]), ('h', Trie False [('e', Trie True []), ('a', Trie False [])])]

{- List of tests for `prune` -}
listTestsPrune :: [Bool]
listTestsPrune = [testPrune1, testPrune2, testPrune3, testPrune4, testPrune5, testPrune6, testPrune7]

{- Run all tests for `prune` -}
allTestsPrune :: Bool
allTestsPrune = all (== True) listTestsPrune


-----------------
----- check -----
-----------------

{- Empty Trie -}
testCheck1 :: Bool
testCheck1 = check trie "" == False && check trie "a" == False
    where
        trie = Trie False []

{- Empty string -}
testCheck2 :: Bool
testCheck2 = check trie "" == True && check trie "a" == False
    where
        trie = Trie True []

{- Non-empty Trie contains string max depth -}
testCheck3 :: Bool
testCheck3 = check trie "hello" == True
    where
        trie = Trie False [('h', Trie False [('e', Trie True [('l', Trie False [('l', Trie True [('o', Trie True [])])])])])]

{- Non-empty Trie contains string partial depth -}
testCheck4 :: Bool
testCheck4 = check trie "he" == True && check trie "hell" == True
    where
        trie = Trie False [('h', Trie False [('e', Trie True [('l', Trie False [('l', Trie True [('o', Trie True [])])])])])]

{- Non-empty Trie doesn't contain string -}
testCheck5 :: Bool
testCheck5 = check trie "h" == False && check trie "hel" == False && check trie "helloo" == False
    where
        trie = Trie False [('h', Trie False [('e', Trie True [('l', Trie False [('l', Trie True [('o', Trie True [])])])])])]

{- List of tests for `check` -}
listTestsCheck :: [Bool]
listTestsCheck = [testCheck1, testCheck2, testCheck3, testCheck4, testCheck5]

{- Run all tests for `check` -}
allTestsCheck :: Bool
allTestsCheck = all (== True) listTestsCheck


-----------------
----- union -----
-----------------

{- Union empty + empty = empty -}
testUnion1 :: Bool
testUnion1 = compareLists (toList unioned) []
    where
        unioned = union trie trie
        trie = Trie False []

{- Union self + empty = self -}
testUnion2 :: Bool
testUnion2 = compareLists (toList unioned) (toList trie)
    where
        unioned = union trie (Trie False [])
        trie = Trie False [('h', Trie False [('e', Trie True [('l', Trie False [('l', Trie True [('o', Trie True [])])])])])]

{- Union self + self = self -}
testUnion3 :: Bool
testUnion3 = compareLists (toList unioned) (toList trie)
    where
        unioned = union trie trie
        trie = Trie False [('h', Trie False [('e', Trie True [('l', Trie False [('l', Trie True [('o', Trie True [])])])])])]

{- Union a + b no overlap -}
testUnion4 :: Bool
testUnion4 = compareLists (toList unioned) expected
    where
        unioned = union trie1 trie2
        trie1 = fromList ["hello", "hi", "hell", "apple"]
        trie2 = fromList ["him", "he", "bear"]
        expected = ["hello", "hi", "hell", "apple", "him", "he", "bear"]

{- Union a + b with overlap -}
testUnion5 :: Bool
testUnion5 = compareLists (toList unioned) expected
    where
        unioned = union trie1 trie2
        trie1 = fromList ["hello", "hi", "he", "apple"]
        trie2 = fromList ["him", "he", "bear", "apple"]
        expected = ["hello", "hi", "he", "apple", "him", "bear"]

{- List of tests for `union` -}
listTestsUnion :: [Bool]
listTestsUnion = [testUnion1, testUnion2, testUnion3, testUnion4, testUnion5]

{- Run all tests for `union` -}
allTestsUnion :: Bool
allTestsUnion = all (== True) listTestsUnion


------------------------
----- intersection -----
------------------------

{- Intersection empty + empty = empty -}
testIntersection1 :: Bool
testIntersection1 = compareLists (toList intersect) []
    where
        intersect = intersection trie trie
        trie = Trie False []

{- Intersection self + empty = empty -}
testIntersection2 :: Bool
testIntersection2 = compareLists (toList intersect) []
    where
        intersect = intersection trie (Trie False [])
        trie = Trie False [('h', Trie False [('e', Trie True [('l', Trie False [('l', Trie True [('o', Trie True [])])])])])]

{- Intersection self + self = self -}
testIntersection3 :: Bool
testIntersection3 = compareLists (toList intersect) (toList trie)
    where
        intersect = intersection trie trie
        trie = Trie False [('h', Trie False [('e', Trie True [('l', Trie False [('l', Trie True [('o', Trie True [])])])])])]

{- Intersection a + b no overlap -}
testIntersection4 :: Bool
testIntersection4 = compareLists (toList intersect) expected
    where
        intersect = intersection trie1 trie2
        trie1 = fromList ["hello", "hi", "hell", "apple"]
        trie2 = fromList ["him", "he", "bear"]
        expected = []

{- Intersection a + b with overlap -}
testIntersection5 :: Bool
testIntersection5 = compareLists (toList intersect) expected
    where
        intersect = intersection trie1 trie2
        trie1 = fromList ["hello", "hi", "he", "apple"]
        trie2 = fromList ["him", "he", "bear", "apple"]
        expected = ["he", "apple"]

{- List of tests for `intersection` -}
listTestsIntersection :: [Bool]
listTestsIntersection = [testIntersection1, testIntersection2, testIntersection3, testIntersection4, testIntersection5]

{- Run all tests for `intersection` -}
allTestsIntersection :: Bool
allTestsIntersection = all (== True) listTestsIntersection


----------------
----- pick -----
----------------

{- Helper to assert `pick` return value -}
assertPick :: Ord a => [a] -> a -> (Bool, [a]) -> Bool
assertPick xs x (expB, expL) = actB == expB && compareLists actL expL
    where
        (actB, actL) = pick xs x

{- Pick from empty -}
testPick1 :: Bool
testPick1 = assertPick [] 1 (False, []) && assertPick [] "hello" (False, [])

{- Pick only element -}
testPick2 :: Bool
testPick2 = assertPick [2] 2 (True, []) && assertPick ["hello"] "hello" (True, [])

{- Pick from multiple -}
testPick3 :: Bool
testPick3 = assertPick [1, 2, 3] 3 (True, [1, 2]) && assertPick ["his", "he", "him"] "his" (True, ["he", "him"])

{- Pick non-existent -}
testPick4 :: Bool
testPick4 = assertPick [1, 2, 3] 4 (False, [1, 2, 3]) && assertPick ["his", "he", "him"] "hello" (False, ["his", "he", "him"])

{- List of tests for `pick` -}
listTestsPick :: [Bool]
listTestsPick = [testPick1, testPick2, testPick3, testPick4]

{- Run all tests for `pick` -}
allTestsPick :: Bool
allTestsPick = all (== True) listTestsPick


------------------------
----- filterLength -----
------------------------

{- length 0 -}
testFilterLength1 :: Bool
testFilterLength1 = assertTrieList none [] && assertTrieList one [""]
    where
        none = filterLength n (Trie False [])
        one = filterLength n (fromList ["a", "", "orange"])
        n = 0

{- Has words of given length -}
testFilterLength2 :: Bool
testFilterLength2 = assertTrieList actual expected
    where
        actual = filterLength 4 trie
        trie = fromList ["he", "hello", "hell", "make", "makes", "mat", "mate"]
        expected = ["hell", "make", "mate"]

{- No words of given length -}
testFilterLength3 :: Bool
testFilterLength3 = assertTrieList actual []
    where
        actual = filterLength 4 trie
        trie = fromList ["he", "hello", "orange", "makes", "mat"]

{- List of tests for `filterLength` -}
listTestsFilterLength :: [Bool]
listTestsFilterLength = [testFilterLength1, testFilterLength2, testFilterLength3]

{- Run all tests for `filterLength` -}
allTestsFilterLength :: Bool
allTestsFilterLength = all (== True) listTestsFilterLength


-------------------------
----- filterPattern -----
-------------------------

{- no pattern = length 0 -}
testFilterPattern1 :: Bool
testFilterPattern1 = assertTrieList actual expected
    where
        actual = filterPattern pat trie
        pat = []
        trie = fromList ["a", "", "orange"]
        expected = [""]

{- all Wildcard - has words -}
testFilterPattern2 :: Bool
testFilterPattern2 = assertTrieList actual expected
    where
        actual = filterPattern pat trie
        pat = [Wildcard, Wildcard, Wildcard, Wildcard]
        trie = fromList ["he", "hello", "hell", "make", "makes", "mat", "mate"]
        expected = ["hell", "make", "mate"]

{- all Wildcard - no words -}
testFilterPattern3 :: Bool
testFilterPattern3 = assertTrieList actual []
    where
        actual = filterPattern pat trie
        pat = [Wildcard, Wildcard, Wildcard, Wildcard]
        trie = fromList ["he", "hello", "orange", "makes", "mat"]

{- all Constraint - exact -}
testFilterPattern4 :: Bool
testFilterPattern4 = assertTrieList actual1 ["his"] && assertTrieList actual2 ["hi"]
    where
        actual1 = filterPattern pat1 trie
        pat1 = [Mem "h", Mem "i", Mem "s"]
        actual2 = filterPattern pat2 trie
        pat2 = [Mem "h", Mem "i"]
        trie = fromList ["he", "hello", "hell", "hi", "hit", "his"]

{- all Constraint - varies -}
testFilterPattern5 :: Bool
testFilterPattern5 = assertTrieList actual1 expected1 && assertTrieList actual2 expected2
    where
        actual1 = filterPattern pat1 trie
        pat1 = [Mem "hm", Mem "ei", Mem "st"]
        expected1 = ["hit", "his", "met"]
        actual2 = filterPattern pat2 trie
        pat2 = [Mem "hm", Mem "a", Mem "t", Mem "es"]
        expected2 = ["mate", "mats", "hats", "hate"]
        trie = fromList ["he", "hello", "hell", "hi", "hit", "his", "mate", "mats", "mat", "met", "has", "hats", "hater", "hate", "mite"]

{- Wildcard + Constraint mix -}
testFilterPattern6 :: Bool
testFilterPattern6 = assertTrieList actual1 expected1 && assertTrieList actual2 expected2
    where
        actual1 = filterPattern pat1 trie
        pat1 = [Mem "hm", Wildcard, Mem "st"]
        expected1 = ["hit", "his", "has", "mat", "met"]
        actual2 = filterPattern pat2 trie
        pat2 = [Mem "hm", Wildcard, Mem "t", Wildcard]
        expected2 = ["hots", "mate", "mats", "hats", "hate", "mite"]
        trie = fromList ["he", "hello", "hots", "hi", "hit", "his", "mate", "mats", "mat", "met", "has", "hats", "hater", "hate", "mite"]

{- List of tests for `filterPattern` -}
listTestsFilterPattern :: [Bool]
listTestsFilterPattern = [testFilterPattern1, testFilterPattern2, testFilterPattern3, testFilterPattern4, testFilterPattern5, testFilterPattern6]

{- Run all tests for `filterPattern` -}
allTestsFilterPattern :: Bool
allTestsFilterPattern = all (== True) listTestsFilterPattern


---------------------------
----- filterPlayables -----
---------------------------

{- Empty rack -}
testFilterPlayables1 :: Bool
testFilterPlayables1 = assertTrieList actual []
    where
        actual = filterPlayables [] trie
        trie = fromList ["he", "hello", "hots"]

{- 1 tile rack -}
testFilterPlayables2 :: Bool
testFilterPlayables2 = assertTrieList actual1 ["a", "o"] && assertTrieList actual2 ["a"]
    where
        actual1 = filterPlayables [Blank] trie
        actual2 = filterPlayables [Letter 'a'] trie
        trie = fromList ["a", "he", "at", "am", "o", "orange"]

{- Rack all blanks -}
testFilterPlayables3 :: Bool
testFilterPlayables3 = assertTrieList actual expected
    where
        actual = filterPlayables r trie
        r = [Blank, Blank, Blank, Blank]
        trie = fromList ["he", "hello", "hell", "make", "makes", "mat", "mate"]
        expected = ["he", "hell", "make", "mat", "mate"]

{- Rack all letters, find anagrams of same length -}
testFilterPlayables4 :: Bool
testFilterPlayables4 = assertTrieList actual expected
    where
        actual = filterPlayables r trie
        r = [Letter 'a', Letter 't', Letter 'e']
        trie = fromList ["ate", "eat", "eta", "tea", "apt", "erm", "top", "tap"]
        expected = ["ate", "eat", "eta", "tea"]

{- Rack all letters, find anagrams of different length -}
testFilterPlayables5 :: Bool
testFilterPlayables5 = assertTrieList actual expected
    where
        actual = filterPlayables r trie
        r = [Letter 'a', Letter 't', Letter 'e']
        trie = fromList ["ate", "eat", "age", "eta", "tea", "ae", "he", "at", "eaten", "teapot", "hate", "hi", "aw", "maw"]
        expected = ["ate", "eat", "eta", "tea", "ae", "at"]

{- Rack mix blank + letters, find all anagrams -}
testFilterPlayables6 :: Bool
testFilterPlayables6 = assertTrieList actual expected
    where
        actual = filterPlayables r trie
        r = [Letter 'a', Blank, Letter 'e']
        trie = fromList ["ate", "eat", "age", "eta", "tea", "ae", "he", "at", "eaten", "teapot", "hate", "hi", "aw", "maw"]
        expected = ["ate", "eat", "age", "eta", "tea", "ae", "he", "at", "aw"]

{- List of tests for `filterPlayables` -}
listTestsFilterPlayables :: [Bool]
listTestsFilterPlayables = [testFilterPlayables1, testFilterPlayables2, testFilterPlayables3, testFilterPlayables4, testFilterPlayables5, testFilterPlayables6]

{- Run all tests for `filterPlayables` -}
allTestsFilterPlayables :: Bool
allTestsFilterPlayables = all (== True) listTestsFilterPlayables


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
