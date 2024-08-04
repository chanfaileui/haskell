module QuizMarkerTests where

import Data.Char (isDigit)
import QuizMarker
import Test.QuickCheck

-- Tests for first
firstTest1 = runParserPartial (first [keyword "a", keyword "b"]) "abc" == Just ("bc", ())

firstTest2 = runParserPartial (first [keyword "b", keyword "a"]) "abc" == Just ("bc", ())

firstTest3 = runParserPartial (first [keyword "c", keyword "d"]) "abc" == Nothing

firstTests = [firstTest1, firstTest2, firstTest3]

allFirstTestsPass = all (== True) firstTests

-- Tests for peekChar
peekCharTest1 = runParserPartial peekChar "abc" == Just ("abc", 'a')

peekCharTest2 = runParserPartial peekChar "" == Nothing

peekCharTests = [peekCharTest1, peekCharTest2]

allPeekCharTestsPass = all (== True) peekCharTests

-- Tests for parseChar
parseCharTest1 = runParserPartial parseChar "abc" == Just ("bc", 'a')

parseCharTest2 = runParserPartial parseChar "" == Nothing

parseCharTests = [parseCharTest1, parseCharTest2]

allParseCharTestsPass = all (== True) parseCharTests

-- Tests for whiteSpace
whiteSpaceTest1 = runParserPartial whiteSpace "  \t\n abc" == Just ("abc", ())

whiteSpaceTest2 = runParserPartial whiteSpace "abc" == Just ("abc", ())

whiteSpaceTests = [whiteSpaceTest1, whiteSpaceTest2]

allWhiteSpaceTestsPass = all (== True) whiteSpaceTests

-- Tests for parseBool
parseBoolTest1 = runParserPartial parseBool "true" == Just ("", True)

parseBoolTest2 = runParserPartial parseBool "false" == Just ("", False)

parseBoolTest3 = runParserPartial parseBool "other" == Nothing

parseBoolTests = [parseBoolTest1, parseBoolTest2, parseBoolTest3]

allParseBoolTestsPass = all (== True) parseBoolTests

-- Tests for parsePositiveInt
parsePositiveIntTest1 = runParserPartial parsePositiveInt "123ab" == Just ("ab", 123)

parsePositiveIntTest2 = runParserPartial parsePositiveInt "AB123ab" == Nothing

parsePositiveIntTest3 = runParserPartial parsePositiveInt "-123ab" == Nothing

parsePositiveIntTest4 = runParserPartial parsePositiveInt "00100" == Just ("", 100)

parsePositiveIntTest5 = runParserPartial parsePositiveInt "0000" == Nothing

parsePositiveIntTest6 = runParserPartial parsePositiveInt "0" == Nothing

parsePositiveIntTest7 = runParserPartial parsePositiveInt "42xyz" == Just ("xyz", 42)

parsePositiveIntTest8 = runParserPartial parsePositiveInt "" == Nothing

parsePositiveIntTests =
  [ parsePositiveIntTest1,
    parsePositiveIntTest2,
    parsePositiveIntTest3,
    parsePositiveIntTest4,
    parsePositiveIntTest5,
    parsePositiveIntTest6,
    parsePositiveIntTest7,
    parsePositiveIntTest8
  ]

allParsePositiveIntTestsPass = all (== True) parsePositiveIntTests

-- Tests for parseDouble
parseDoubleTest1 = runParserPartial parseDouble "123.45" == Just ("", 123.45)

parseDoubleTest2 = runParserPartial parseDouble "-123.45" == Just ("", -123.45)

parseDoubleTest3 = runParserPartial parseDouble "123" == Just ("", 123.0)

parseDoubleTest4 = runParserPartial parseDouble "0.123" == Just ("", 0.123)

parseDoubleTest5 = runParserPartial parseDouble "abc" == Nothing

parseDoubleTests = [parseDoubleTest1, parseDoubleTest2, parseDoubleTest3, parseDoubleTest4, parseDoubleTest5]

allParseDoubleTestsPass = all (== True) parseDoubleTests

-- Tests for parseString
parseStringTest1 = runParserPartial parseString "\"abc\"" == Just ("", "abc")

parseStringTest2 = runParserPartial parseString "\"a\\\"b\"" == Just ("", "a\"b")

parseStringTest3 = runParserPartial parseString "\"abc" == Nothing

parseStringTest4 = runParserPartial parseString "\"a\\nb\"" == Just ("", "a\nb")

parseStringTest5 = runParserPartial parseString "\"\"" == Just ("", "")

parseStringTest6 = runParserPartial parseString "\n" == Nothing

parseStringTests = [parseStringTest1, parseStringTest2, parseStringTest3, parseStringTest4, parseStringTest5, parseStringTest6]

allParseStringTestsPass = all (== True) parseStringTests

-- Tests for parseList
parseListTest1 = runParserPartial (parseList '[' ']' parseBool) "[true, true, false]" == Just ("", [True, True, False])

parseListTest2 = runParserPartial (parseList '[' ']' parseBool) "[ true , false , false ]" == Just ("", [True, False, False])

parseListTest3 = runParserPartial (parseList '[' ']' parseBool) "[true, false, 3]" == Nothing

parseListTest4 = runParserPartial (parseList '[' ']' parseBool) "[true, false,]" == Nothing

parseListTest5 = runParserPartial (parseList '[' ']' parseBool) "[, true, false]" == Nothing

parseListTest6 = runParserPartial (parseList '[' ']' parseBool) "[]" == Just ("", [])

parseListTest7 = runParserPartial (parseList '[' ']' parseBool) "[true]" == Just ("", [True])

parseListTest8 = runParserPartial (parseList '[' ']' parseBool) "[ true ]" == Just ("", [True])

parseListTests =
  [ parseListTest1,
    parseListTest2,
    parseListTest3,
    parseListTest4,
    parseListTest5,
    parseListTest6,
    parseListTest7,
    parseListTest8
  ]

allParseListTestsPass = all (== True) parseListTests

-- Aggregating all test results
allTestsPass =
  and
    [ allFirstTestsPass,
      allPeekCharTestsPass,
      allParseCharTestsPass,
      allWhiteSpaceTestsPass,
      allParseBoolTestsPass,
      allParsePositiveIntTestsPass,
      allParseDoubleTestsPass,
      allParseStringTestsPass,
      allParseListTestsPass
    ]

-- Main function to run all tests
main :: IO ()
main = do
  putStrLn $ "All tests pass: " ++ show allTestsPass
  putStrLn $ "First tests: " ++ show allFirstTestsPass
  putStrLn $ "PeekChar tests: " ++ show allPeekCharTestsPass
  putStrLn $ "ParseChar tests: " ++ show allParseCharTestsPass
  putStrLn $ "WhiteSpace tests: " ++ show allWhiteSpaceTestsPass
  putStrLn $ "ParseBool tests: " ++ show allParseBoolTestsPass
  putStrLn $ "ParsePositiveInt tests: " ++ show allParsePositiveIntTestsPass
  putStrLn $ "ParseDouble tests: " ++ show allParseDoubleTestsPass
  putStrLn $ "ParseString tests: " ++ show allParseStringTestsPass
  putStrLn $ "ParseList tests: " ++ show allParseListTestsPass
