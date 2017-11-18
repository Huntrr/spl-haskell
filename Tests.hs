module Tests where

import Test.HUnit (runTestTT,Test(..),Assertion, (~?=), (~:), assert)
import Test.QuickCheck (Arbitrary(..), Testable(..), Gen, elements,
  oneof, frequency, sized, quickCheckWith, stdArgs, maxSize,
  classify,  maxSuccess, listOf, resize, scale, (==>))

import LanguageParser

main :: IO ()
main = do
   _ <- runTestTT (TestList [])
   putStrLn "Testing Roundtrip property..."
   quickCheckN 500 prop_roundtrip
   return ()

------------ Sample Programs -----------------
-- TODO

------------ HUnit Tests ---------------------
-- TODO

------------- Roundtrip property -------------
prop_roundtrip :: Program -> Bool
prop_roundtrip s = P.parse programP (indented s) == Right s

-- TODO program equivalency with N steps

------------- Arbitrary Instance -------------
-- TODO
