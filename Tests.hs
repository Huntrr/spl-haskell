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
samplePrograms = [ ("hello", [], "Hello World!")
                 , ("primes", [], "12357111315")
                 , ("reverse", ['a', 'b', 'c', 'd', -1], "dcba") ]

-- TODO: Run all these programs with given inputs and check for outputs

------------ HUnit Tests ---------------------
-- TODO: PARSER
-- Unknown vocabulary

-- TODO: PRETTY PRINTER

-- TODO: EVALUATOR
-- Mostly handled with samplePrograms tests, but should also test edge cases
-- and errors!

-- EmptyStack
-- Divide by Zero
-- Goto unknown scene/act
-- Calling out of scope variable
-- Ambiguous assignment
-- Bad if statement


------------- QUICKCHECK --------------------
------------- Roundtrip property -------------
prop_roundtrip :: Program -> Bool
prop_roundtrip s = P.parse programP (indented s) == Right s
-- ^^ TODO THIS WON'T WORK
-- (Constant 7
--       => "the sum of a large angry red king and a rat"
--       => Sum (Constant 8) (Constant (-1)))
-- Could check bounded program equivalency instead?

-- TODO program equivalency with N steps (using stepper)

-- TODO optimizers don't change programs (check bounded computatioN)
-- check for sound/correct optimizations

-- TODO optimizers make ASTs smaller
-- Could we also profile evaluations to see if optimizers make programs faster?

-- TODO any literal constant can be pretty printed
-- i.e. (Constant 7) pretty printed and parsed will not be (Constant 7) but
-- should still EVALUATE to (7) (b/c all literals are powers of 2)

------------- Arbitrary Instance -------------
-- TODO
