{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction,
    FlexibleInstances #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}

module Tests where

import Test.HUnit (runTestTT,Test(..),Assertion, (~?=), (~:), assert,
  assertFailure)
import Test.QuickCheck (Arbitrary(..), Testable(..), Gen, elements,
  oneof, frequency, sized, quickCheckWith, stdArgs, maxSize,
  classify,  maxSuccess, listOf, resize, scale, (==>))

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

import LanguageParser
import AST
import Evaluator
import PrettyPrinter

main :: IO ()
main = do
   _ <- runTestTT (TestList [])
   putStrLn "Testing Roundtrip property..."
   quickCheckN 500 prop_roundtrip
   return ()

quickCheckN :: Test.QuickCheck.Testable prop => Int -> prop -> IO ()
quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n , maxSize = 100 }


raises :: Program -> Exception -> Test
s `raises` v = case (execute s emptyState) of
   (Left v',_) -> v ~?= v'
   _            -> TestCase $ assertFailure "Error in raises"


------------ Sample Programs -----------------
samplePrograms = [ ("hello", [], "Hello World!")
                 , ("primes", [], "12357111315")
                 , ("reverse", ['a', 'b', 'c', 'd', '0'], "dcba") ]

-- TODO: Run all these programs with given inputs and check for outputs

------------ HUnit Tests ---------------------
-- TODO: PARSER
-- Unknown vocabulary

helloWorldHeader = Header
                   "The Infamous Hello World Program."
                   [
                      Character "Romeo" "a young man with a remarkable patience.",
                      Character "Juliet" "a likewise young woman of remarkable grace.",
                      Character "Ophelia" "a remarkable woman much in dispute with Hamlet.",
                      Character "Hamlet" "the flatterer of Andersen Insulting A/S."
                    ]

headerString = "The Infamous Hello World Program.\n   \
                          \Romeo,    a young man with a remarkable patience.\n\
                          \Juliet, a likewise young woman of remarkable grace.   \n\
                          \   Ophelia, a remarkable woman much in dispute with Hamlet.\n\
                          \Hamlet,the flatterer of Andersen Insulting A/S.\n"

testParseHeader :: Test
testParseHeader =
  TestList
    [
    P.parse headerP "" headerString ~?= Right helloWorldHeader
    ]

testParseConstant :: Test
testParseConstant =
  TestList
    [
      P.parse constantP "" "my Flower" ~?= Right (Constant 1),
      P.parse constantP "" "an amazing Flower" ~?= Right (Constant 2),
      P.parse constantP "" "an amazing amazing Flower" ~?= Right (Constant 4),
      P.parse constantP "" "a Pig" ~?= Right (Constant (-1)),
      P.parse constantP "" "my amazing Pig" ~?= Right (Constant (-2)),
      P.parse constantP "" "your amazing amazing Pig" ~?= Right (Constant (-4))
    ]

testParseExpression :: Test
testParseExpression =
  TestList
    [
      P.parse expressionP "" "my little pony" ~?= Right (Constant 2),
      P.parse expressionP "" "the difference between my little pony and your big hairy hound"
        ~?= Right (Difference (Constant 2) (Constant 4)),
      P.parse expressionP "" "the cube of your sorry little codpiece"
        ~?= Right (Cube (Constant (-4))),
      P.parse expressionP "" "lying stupid fatherless big smelly half-witted corward"
        ~?= Right (Constant (-64)),
      P.parse expressionP "" "the difference between the square of the difference between my little pony and your big hairy hound and the cube of your sorry little codpiece"
        ~?= Right (Difference (Square (Difference (Constant 2) (Constant 4))) (Cube (Constant (-4)))),
      P.parse expressionP "" "Juliet" ~?= Right (Var "Juliet"),
      -- TODO: multi-word variables don't work yet
      P.parse expressionP "" "the cube of the Ghost" ~?= Right (Cube (Var "the Ghost")),
      P.parse expressionP "" "the product of Juliet and a Pig"
        ~?= Right (Product (Var "Juliet") (Constant (-1))),
      P.parse expressionP "" "the difference between Juliet and thyself"
        ~?= Right (Difference (Var "Juliet") (Var "thyself")),
      P.parse expressionP "" "the difference between the square of Juliet and thyself"
        ~?= Right (Difference (Square (Var "Juliet")) (Var "thyself")),
      P.parse expressionP "" "the difference between the square root of Juliet and thyself"
        ~?= Right (Difference (SquareRoot (Var "Juliet")) (Var "thyself")),
      P.parse expressionP "" "the difference between the square root of Juliet and twice thyself"
        ~?= Right (Difference (SquareRoot (Var "Juliet")) (Twice (Var "thyself")))
    ]
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
prop_roundtrip s = P.parse programP "" (render s) == Right s
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

instance Arbitrary Program where
  arbitrary = undefined
  shrink = undefined
