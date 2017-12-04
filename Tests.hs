{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances      #-}
-- TODO: remove -fdefer-type-errors
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -fdefer-type-errors #-}

module Tests where

import           Test.HUnit           (Assertion, Test (..), assert,
                                       assertFailure, runTestTT, (~:), (~?=))
import           Test.QuickCheck      (Arbitrary (..), Gen, Testable (..),
                                       classify, elements, frequency, listOf,
                                       maxSize, maxSuccess, oneof,
                                       quickCheckWith, resize, scale, sized,
                                       stdArgs, (==>))

import           Data.Map             (Map)
import qualified Data.Map             as Map
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as P

import           AST
import           Data.Either.Extra
import           Evaluator
import           LanguageParser
import           PrettyPrinter

main :: IO ()
main = do
   _ <- runTestTT (TestList [testParseHeader, testParseConstant,
                             testParseExpression, testParseComparison,
                             testParseSentence])
   putStrLn "Testing Roundtrip property..."
   quickCheckN 500 prop_roundtrip
   return ()

quickCheckN :: Test.QuickCheck.Testable prop => Int -> prop -> IO ()
quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n , maxSize = 100 }


raises :: Program -> Exception -> Test
s `raises` v = case (execute s emptyState) of
   (Left v',_) -> v ~?= v'
   _           -> TestCase $ assertFailure "Error in raises"


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
      P.parse constantP "" "Your amazing amazing Pig" ~?= Right (Constant (-4)),
      P.parse constantP "" "amazing amazing Pig" ~?= Right (Constant (-4)),
      P.parse constantP "" "a beautiful fair warm peaceful sunny toad's summer"
        ~?= Right (Constant 32)
    ]

testParseExpression :: Test
testParseExpression =
  TestList
    [
      P.parse expressionP "" "my little pony" ~?= Right (Constant 2),
      P.parse expressionP "" "the difference between my little pony and your big hairy hound"
        ~?= Right (Difference (Constant 2) (Constant (-4))),
      P.parse expressionP "" "the cube of your sorry little codpiece"
        ~?= Right (Cube (Constant (-4))),
      P.parse expressionP "" "lying stupid fatherless big smelly half-witted coward"
        ~?= Right (Constant (-64)),
      P.parse expressionP "" "the difference between the square of the difference between my little pony and your big hairy hound and the cube of your sorry little codpiece"
        ~?= Right (Difference (Square (Difference (Constant 2) (Constant (-4)))) (Cube (Constant (-4)))),
      P.parse expressionP "" "Juliet" ~?= Right (Var (They "Juliet")),
      -- TODO: multi-word variables don't work yet
      P.parse expressionP "" "the cube of the Ghost" ~?= Right (Cube (Var (They "the Ghost"))),
      P.parse expressionP "" "the product of Juliet and a Pig"
        ~?= Right (Product (Var (They "Juliet")) (Constant (-1))),
      P.parse expressionP "" "the difference between Juliet and thyself"
        ~?= Right (Difference (Var (They "Juliet")) (Var You)),
      P.parse expressionP "" "the difference between the square of Juliet and thyself"
        ~?= Right (Difference (Square (Var (They "Juliet"))) (Var You)),
      P.parse expressionP "" "the difference between the square root of Juliet and thyself"
        ~?= Right (Difference (SquareRoot (Var (They "Juliet"))) (Var You)),
      P.parse expressionP "" "the difference between the square root of Juliet and twice thyself"
        ~?= Right (Difference (SquareRoot (Var (They "Juliet"))) (Twice (Var You))),
      P.parse expressionP "" "the remainder of the quotient between Romeo and twice me"
        ~?= Right (Mod (Var (They "Romeo")) (Twice (Var Me)))
    ]

testParseComparison :: Test
testParseComparison =
  TestList
    [
      P.parse comparisonP "" "Am I as good as you?"
        ~?= Right (Comparison E (Var Me) (Var You)),
      P.parse comparisonP "" "Is summer as good as thee?"
        ~?= Right (Comparison E (Constant 1) (Var You)),
      P.parse comparisonP "" "Am I worse than you?"
        ~?= Right (Comparison Lt (Var Me) (Var You)),
      P.parse comparisonP "" "Is a disgusting leech uglier than thee?"
        ~?= Right (Comparison Lt (Constant (-2)) (Var You)),
      P.parse comparisonP "" "Am I better than you?"
        ~?= Right (Comparison Gt (Var Me) (Var You)),
      P.parse comparisonP "" "Is a disgusting leech better than thee?"
        ~?= Right (Comparison Gt (Constant (-2)) (Var You)),
      P.parse comparisonP "" "Am I not as good as you?"
        ~?= Right (Comparison Ne (Var Me) (Var You)),
      P.parse comparisonP "" "Is summer not as good as thee?"
        ~?= Right (Comparison Ne (Constant 1) (Var You)),
      P.parse comparisonP "" "Am I not worse than you?"
        ~?= Right (Comparison Ge (Var Me) (Var You)),
      P.parse comparisonP "" "Is a disgusting leech not uglier than thee?"
        ~?= Right (Comparison Ge (Constant (-2)) (Var You)),
      P.parse comparisonP "" "Am I not better than you?"
        ~?= Right (Comparison Le (Var Me) (Var You)),
      P.parse comparisonP "" "Is a disgusting leech not better than thee?"
        ~?= Right (Comparison Le (Constant (-2)) (Var You)),
      P.parse comparisonP "" "Art thou more cunning than the Ghost?"
        ~?= Right (Comparison Gt (Var You) (Var (They "the Ghost")))
    ]

parseUnwrap :: String -> Sentence
parseUnwrap s = (fst . fromRight') (P.parse sentenceP "" s)

testParseSentence :: Test
testParseSentence =
  TestList
    [
      parseUnwrap "Am I as good as you?" ~?=
        Conditional (Comparison E (Var Me) (Var You)),
      parseUnwrap "Am I as good as nothing?" ~?=
        Conditional (Comparison E (Var Me) (Constant 0)),
      parseUnwrap "Let us proceed to act III." ~?=
        GotoAct 3,
      parseUnwrap "If so, let us proceed to scene III." ~?=
        IfSo (GotoScene 3),
      parseUnwrap "Open your heart!" ~?= OutputNumber,
      parseUnwrap "Speak your mind!" ~?= OutputCharacter,
      parseUnwrap "Listen to your heart." ~?= InputNumber,
      parseUnwrap "Open your mind!" ~?= InputCharacter,
      parseUnwrap "You lying stupid fatherless big smelly half-witted coward!"
        ~?= Declaration (Constant (-64)),
      parseUnwrap "You are as stupid as the difference between Juliet and thyself."
        ~?= Declaration (Difference (Var (They "Juliet")) (Var You)),
      parseUnwrap "You are my little pony!"
        ~?= Declaration (Constant 2),
      parseUnwrap "You are nothing!"
        ~?= Declaration (Constant 0),
      parseUnwrap "Remember me!" ~?= Push Me,
      parseUnwrap "Remember Hamlet!" ~?= Push (They "Hamlet"),
      parseUnwrap "Recall your unhappy childhood!" ~?= Pop
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
