import Test.QuickCheck
import Data.Char (isAlpha, toUpper, ord)

-- Given rot13 function
rot13 :: String -> String
rot13 = map $ \x -> 
          case lookup x table of
            Just y  -> y 
            Nothing -> x
  where
    table = table' 'A' 'Z' ++ table' 'a' 'z'
    table' a z = zip [a..z] (drop 13 (cycle [a..z]))

-- Property (a): length preservation
prop_LengthPreservation :: String -> Bool
prop_LengthPreservation x = length x == length (rot13 x)

-- Property (d): non-alphabetic characters
prop_NonAlphaUnchanged :: String -> Property
prop_NonAlphaUnchanged x = all (not . isAlpha) x ==> rot13 x == x

-- Property (e): concatenation preservation
prop_ConcatPreservation :: String -> String -> Bool
prop_ConcatPreservation a b = rot13 (a ++ b) == rot13 a ++ rot13 b

-- Property (g): involution
prop_Involution :: String -> Bool
prop_Involution x = rot13 (rot13 x) == x

-- Main function to run QuickCheck tests
main :: IO ()
main = do
  quickCheck prop_LengthPreservation
  quickCheck prop_NonAlphaUnchanged
  quickCheck prop_ConcatPreservation
  quickCheck prop_Involution
