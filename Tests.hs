{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE TupleSections      #-}

module Tests where

import           Test.HUnit           (Assertion, Test (..), assert,
                                       assertFailure, runTestTT, (~:), (~?=),
                                       (@?=))
import           Test.QuickCheck      (Arbitrary (..), Gen, Testable (..),
                                       classify, elements, frequency, listOf,
                                       maxSize, maxSuccess, oneof,
                                       quickCheckWith, resize, scale, sized,
                                       stdArgs, (==>), choose, generate,
                                       sublistOf)

import Debug.Trace
import Data.Char (ord, chr)

import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Set             (Set)
import qualified Data.Set             as Set
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as P

import           AST
import           Data.Either.Extra
import           Evaluator
import           LanguageParser
import           PrettyPrinter
import           Main
import qualified WordLists            as W
import Text.PrettyPrint (render)

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

file f = "samples/" ++ f ++ ".spl"

raises :: String -> [Int] -> Exception -> Test
raises p i v = p ~: TestCase $ do
  result <- evaluateFile (file p) i
  case result of
    (Right e) -> e @?= v
    (Left _)  -> assertFailure "No exception"

outputs :: String -> [Int] -> String -> Test
outputs p i o = p ~: TestCase $ do
  result <- evaluateFile (file p) i
  case result of
    (Right _) -> assertFailure "Exception"
    (Left v)  -> v @?= o


------------ HUnit Tests ---------------------
-- TODO: PARSER
-- Unknown vocabulary

helloWorldHeader = Header
                   "The Infamous Hello World Program."
                   [
                      Character "romeo" "a young man with a remarkable patience.",
                      Character "juliet" "a likewise young woman of remarkable grace.",
                      Character "ophelia" "a remarkable woman much in dispute with Hamlet.",
                      Character "hamlet" "the flatterer of Andersen Insulting A/S."
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
      P.parse expressionP "" "Juliet" ~?= Right (Var (They "juliet")),
      -- TODO: multi-word variables don't work yet
      P.parse expressionP "" "the cube of the Ghost" ~?= Right (Cube (Var (They "the ghost"))),
      P.parse expressionP "" "the product of Juliet and a Pig"
        ~?= Right (Product (Var (They "juliet")) (Constant (-1))),
      P.parse expressionP "" "the difference between Juliet and thyself"
        ~?= Right (Difference (Var (They "juliet")) (Var You)),
      P.parse expressionP "" "the difference between the square of Juliet and thyself"
        ~?= Right (Difference (Square (Var (They "juliet"))) (Var You)),
      P.parse expressionP "" "the difference between the square root of Juliet and thyself"
        ~?= Right (Difference (SquareRoot (Var (They "juliet"))) (Var You)),
      P.parse expressionP "" "the difference between the square root of Juliet and twice thyself"
        ~?= Right (Difference (SquareRoot (Var (They "juliet"))) (Twice (Var You))),
      P.parse expressionP "" "the remainder of the quotient between Romeo and twice me"
        ~?= Right (Mod (Var (They "romeo")) (Twice (Var Me)))
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
      P.parse comparisonP "" "Art thou more cunning than the ghost?"
        ~?= Right (Comparison Gt (Var You) (Var (They "the ghost")))
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
        ~?= Declaration (Difference (Var (They "juliet")) (Var You)),
      parseUnwrap "You are my little pony!"
        ~?= Declaration (Constant 2),
      parseUnwrap "You are nothing!"
        ~?= Declaration (Constant 0),
      parseUnwrap "Remember me!" ~?= Push Me,
      parseUnwrap "Remember Hamlet!" ~?= Push (They "hamlet"),
      parseUnwrap "Recall your unhappy childhood!" ~?= Pop,
      parseUnwrap "You are a good fat-kidneyed trustworthy blister."
        ~?= Declaration (Constant (-8))
    ]

-- TODO: PRETTY PRINTER


------------ EVALUATOR ----------------------
evaluatorTests :: Test
evaluatorTests = TestList [
    sampleTest
  ]
------------ Sample Programs -----------------
samplePrograms = [ ("hello", [], "Hello World!\n")
                 , ("primes", [20], ">2\n3\n5\n7\n11\n13\n17\n19\n")
                 , ("reverse", toInts "abcdef\n", "fedcba")
                 , ("bottles", [], "15 bottles of beer on the wall, 15 bottles of beer.\r\nTake one down, pass it around, 14 bottles of beer on the wall.\r\n14 bottles of beer on the wall, 14 bottles of beer.\r\nTake one down, pass it around, 13 bottles of beer on the wall.\r\n13 bottles of beer on the wall, 13 bottles of beer.\r\nTake one down, pass it around, 12 bottles of beer on the wall.\r\n12 bottles of beer on the wall, 12 bottles of beer.\r\nTake one down, pass it around, 11 bottles of beer on the wall.\r\n11 bottles of beer on the wall, 11 bottles of beer.\r\nTake one down, pass it around, 10 bottles of beer on the wall.\r\n10 bottles of beer on the wall, 10 bottles of beer.\r\nTake one down, pass it around, 9 bottles of beer on the wall.\r\n9 bottles of beer on the wall, 9 bottles of beer.\r\nTake one down, pass it around, 8 bottles of beer on the wall.\r\n8 bottles of beer on the wall, 8 bottles of beer.\r\nTake one down, pass it around, 7 bottles of beer on the wall.\r\n7 bottles of beer on the wall, 7 bottles of beer.\r\nTake one down, pass it around, 6 bottles of beer on the wall.\r\n6 bottles of beer on the wall, 6 bottles of beer.\r\nTake one down, pass it around, 5 bottles of beer on the wall.\r\n5 bottles of beer on the wall, 5 bottles of beer.\r\nTake one down, pass it around, 4 bottles of beer on the wall.\r\n4 bottles of beer on the wall, 4 bottles of beer.\r\nTake one down, pass it around, 3 bottles of beer on the wall.\r\n3 bottles of beer on the wall, 3 bottles of beer.\r\nTake one down, pass it around, 2 bottles of beer on the wall.\r\n2 bottles of beer on the wall, 2 bottles of beer.\r\nTake one down, pass it around, 1 bottle of beer on the wall.\r\n1 bottle of beer on the wall, 1 bottle of beer.\r\nTake one down, pass it around, 0 bottles of beer on the wall.\r\n")
                 , ("fibonacci2", [], "0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n55\n89\n144\n233\n377\n610\n987\n1597\n2584\n4181\n6765\n10946\n17711\n28657\n46368\n75025\n121393\n196418\n317811\n514229\n832040\n1346269\n2178309\n3524578\n5702887\n9227465\n14930352\n24157817\n39088169\n63245986\n102334155\n165580141\n267914296\n433494437\n701408733\n")
                 , ("fibonacci", [], "0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n55\n89\n144\n233\n377\n610\n987\n1597\n2584\n4181\n6765\n10946\n17711\n28657\n46368\n75025\n121393\n196418\n317811\n514229\n832040\n1346269\n2178309\n3524578\n5702887\n9227465\n14930352\n24157817\n39088169\n63245986\n102334155\n165580141\n267914296\n433494437\n701408733")
                 , ("guess", toInts "=", "500?\r\n500\r\n")
                 , ("guess", toInts ">><<>=", "500?\r\n750?\r\n875?\r\n812?\r\n781?\r\n796?\r\n796\r\n")
                 ]

toInts = map f where
  f '\n' = -1
  f c    = ord c

sampleTest :: Test
sampleTest = TestList $ map f samplePrograms where
  f (fn, i, o) = outputs fn i o

-- Mostly handled with samplePrograms tests, but should also test edge cases
-- and errors!


------------- QUICKCHECK --------------------
------------- Roundtrip property -------------
prop_roundtrip :: Program -> Bool
prop_roundtrip s = undefined
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
prop_constant :: Value -> IO Bool
prop_constant c = do
  d <- pp (Constant c)
  return $ P.parse expressionP "" (render d) == Right (Constant c)


------------- Arbitrary Instance -------------
number :: [a] -> [(Int, a)]
number = zip [1..]

chars = ["Romeo", "Juliet", "Hamlet", "Ophelia", "Othello", "Puck", "The Ghost"]
arbCname :: Gen CName
arbCname = elements chars

genSentence :: Int -> Gen Sentence
genSentence n = frequency [
    (6, pure OutputNumber),
    (6, pure OutputCharacter),
    (3, pure InputNumber),
    (3, pure InputCharacter),
    (8, Declaration <$> arbitrary),
    (3, Push <$> arbitrary),
    (1, pure Pop),
    (12, Conditional <$> arbitrary),
    (7, GotoScene <$> choose (1, 6)),
    (7, GotoAct <$> choose (1, 6)),
    (3, IfSo <$> genSentence n'),
    (3, IfNot <$> genSentence n')
  ] where n' = n `div` 2

genComparison :: Int -> Gen Comparison
genComparison n = Comparison <$>
  (elements [Lt, Le, E, Ne, Gt, Ge]) <*> (genExp n') <*> (genExp n')
  where n' = n `div` 2

genExp :: Int -> Gen Expression
genExp n = frequency [
    (1, Constant <$> arbitrary),
    (n', chooseBop <*> genExp n' <*> genExp n'),
    (n', chooseOp <*> genExp n'),
    (1, Var <$> arbitrary)
  ] where 
    n' = n `div` 2
    chooseBop = elements [Sum, Difference, Product, Quotient, Mod]
    chooseOp  = elements [Square, Cube, SquareRoot, Twice]

instance Arbitrary Comparison where
  arbitrary = sized genComparison
  shrink (Comparison r e1 e2) = [Comparison r e1' e2' | e1' <- shrink e1,
                                                        e2' <- shrink e2]

instance Arbitrary Expression where
  arbitrary = sized genExp
  shrink (Constant v)     = [Constant v' | v' <- shrink v]
  shrink (Sum a b)        = [Sum a' b' | a' <- shrink a, b' <- shrink b]
  shrink (Difference a b) = [Difference a' b' | a' <- shrink a, b' <- shrink b]
  shrink (Product a b)    = [Product a' b' | a' <- shrink a, b' <- shrink b]
  shrink (Quotient a b)   = [Quotient a' b' | a' <- shrink a, b' <- shrink b]
  shrink (Square a)       = [Square a' | a' <- shrink a]
  shrink (Cube a)         = [Cube a' | a' <- shrink a]
  shrink (SquareRoot a)   = [SquareRoot a' | a' <- shrink a]
  shrink (Twice a)        = [Twice a' | a' <- shrink a]
  shrink (Mod a b)        = [Mod a' b' | a' <- shrink a, b' <- shrink b]
  shrink (Var r)          = [Var r' | r' <- shrink r]

instance Arbitrary Reference where
  arbitrary = oneof [
      elements [ You, Me ],
      They <$> arbCname
    ]
  shrink _ = []

instance Arbitrary Sentence where
  arbitrary = sized genSentence
  shrink (IfSo s)        = shrink s
  shrink (IfNot s)       = shrink s
  shrink (Declaration e) = [Declaration e' | e' <- shrink e]
  shrink (Push r)        = [Push r' | r' <- shrink r]
  shrink (GotoScene l)   = [GotoScene l' | l' <- shrink l]
  shrink (GotoAct l)     = [GotoAct l' | l' <- shrink l]
  shrink (Conditional c) = [Conditional c' | c' <- shrink c]
  shrink _               = []

-- generates a statement and new stage
genStatement :: Set CName -> Int -> Gen (Set CName, [Statement])
genStatement stage n
  | ns == 0   = genEnter
  | ns == 1   = frequency [(1, genExit), (4, genEnter)]
  | otherwise = frequency [(1, genExit), (2, genEnter), (10, genLine)]
    where n'      = n `div` 2
          ns      = length stage
          list    = Set.toList stage
          genExit = do
            cs <- take 2 <$> sublistOf list
            return $ case cs of
                       [c] -> (Set.delete c stage, [Exit c])
                       _   -> (stage Set.\\ (Set.fromList cs), [Exeunt cs])
          genEnter = do
            cs <- Set.fromList <$> listOf arbCname
            let cs'  = Set.take 2 $ cs Set.\\ stage
             in return (Set.union cs' stage, [Enter (Set.toList cs')])
          genLine = do
            speaker <- elements list
            other   <- elements (Set.toList $ Set.delete speaker stage)
            (stage', before, _) <- disambiguate speaker other
            sentence <- genSentence n'
            return (stage', before ++ [Line speaker sentence])
          disambiguate c1 c2 = let set   = Set.fromList [c1, c2]
                                   rest  = stage Set.\\ set
                                   lrest = Set.toList rest
                                in return (set, Exit <$> lrest,
                                               Enter . (:[]) <$> lrest)

genBlock :: Int -> Gen Block
genBlock n = do
  block <- gen Set.empty n
  return $ (Exeunt [], blankAnnotation) : block
  
  where
    gen stage n = frequency [
        (1, pure []),
        (n', do (stage', burst) <- genStatement stage n'
                rest <- gen stage' n'
                let this = (, blankAnnotation) <$> burst 
                 in return $ this ++ rest
        )
      ] where n' = n `div` 2

instance Arbitrary Statement where
  arbitrary = frequency [
      (12, Enter <$> listOf arbCname),
      (6,  Exit <$> arbCname),
      (1,  Exeunt <$> listOf arbCname),
      (10, Line <$> arbCname <*> sized genSentence)
    ]
  shrink _ = []

instance Arbitrary Annotation where
  arbitrary = elements [blankAnnotation]
  shrink a = [a]

instance Arbitrary Scene where
  arbitrary = Scene "SceneName" <$> sized genBlock
  shrink (Scene d b) = Scene d <$> [take n b | n <- [1..(length b - 1)]]

instance Arbitrary Act where
  arbitrary = Act "ActName" <$> (Map.fromList . number <$> list)
    where
      list = (:) <$> (arbitrary :: Gen Scene) <*> (arbitrary :: Gen [Scene])
  shrink (Act d sceneMap) =
    let list  = snd <$> Map.toList sceneMap
        list' = [ act | act <- shrink list ]
     in Act d . Map.fromList . number <$> take (length list `div` 2) list'

instance Arbitrary Program where
  arbitrary = Program (Header "Much Ado about Monads"
    ((\c -> Character c "a character") <$> chars)) <$>
      (Map.fromList . number <$> arbitrary)
  shrink (Program h actMap) =
    let list  = snd <$> Map.toList actMap
        list' = [ act | act <- shrink list ]
     in Program h . Map.fromList . number <$> take (length list `div` 2) list'
