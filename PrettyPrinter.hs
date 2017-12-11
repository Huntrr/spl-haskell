{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction,
    FlexibleInstances #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}

module PrettyPrinter where
  
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP
import Test.QuickCheck (generate,elements,vectorOf)
import Control.Monad (liftM2,liftM3,foldM)
import WordLists as W
import Text.Numeral.Roman
import Numeric
import qualified Data.Char as Char
import Data.Map as Map
import qualified Data.List as List
import Optimize

{-- NOTE: in order to PrettyPrint an AST printed by the command
`testParse $ "samples/__.spl"` we must:
- replace all occurences of `Pos` with the constructor `mkPos`
- should maybe get rid of newlines in annotations in the
parser's output
- convert the integer labels to `1 :: Label`
- remove all mentions of `fromList` --}

import AST
import LanguageParser
import Evaluator

class PP a where
  pp :: a -> IO Doc

{-- Utilities --}

newline :: Doc
newline = PP.char '\n'

dot :: Doc
dot = PP.char '.'

catSep :: [Doc] -> Doc -> Doc
catSep l sep = List.foldr (\x y -> x PP.<+> sep PP.<+> y) PP.empty l

capName :: String -> String
capName [] = []
capName (h:t) = Char.toUpper h : t

titleIndent :: Doc
titleIndent = PP.text $ replicate 20 ' '

printScene :: String -> Int -> Doc
printScene desc n = titleIndent PP.<> PP.text "Scene" PP.<+> printLabel n
                    PP.<> PP.colon PP.<+> PP.text desc PP.<> newline

printAct :: String -> Int -> Doc
printAct desc n = titleIndent PP.<> PP.text "Act" PP.<+> printLabel n
                    PP.<> PP.colon PP.<+> PP.text desc PP.<> newline

printLabel :: Int -> Doc
printLabel n = toRoman n

printHeader :: Header -> Doc
printHeader (Header title clist) = PP.text title PP.<> newline PP.$$ 
  PP.vcat (printChar <$> clist) PP.<> newline PP.<> newline

printChar :: Character -> Doc
printChar (Character name desc) = PP.text (capName name) PP.<> 
  PP.comma PP.<+> PP.text desc

commandWithCharList :: String -> [CName] -> Doc
commandWithCharList s l = PP.lbrack PP.<> PP.text s PP.<+> chars PP.<> 
                            PP.rbrack PP.<> newline where
  chars = PP.hcat $ PP.punctuate (PP.text " and ") (PP.text . capName <$> l)

isVowel :: Char -> Bool
isVowel x = x `elem` "aeiou"

-- remove the article from a constant if needed
-- e.g. if you is "You", must be "You big fat pony"
fixConstant :: Doc -> Doc -> Doc
fixConstant you ex = let youStr = PP.render you
                         exStr = PP.render ex in
  if (length . words) youStr == 1 then
    you PP.<> PP.text (snd $ break (==' ') exStr)
  else
    you PP.<+> ex

{-- Randomized parts of speech --}

punctuation :: Sentence -> IO Doc
punctuation sentence = case sentence of
  (Conditional _) -> return $ PP.char '?'
  (_) -> do
    c <- generate $ elements ['!', '.']
    return $ PP.char c

areYou :: IO Doc
areYou = do
  ru <- generate $ elements ["Are you", "Art thou"]
  return $ PP.text ru

secondPersonDeclare :: IO Doc
secondPersonDeclare = do
  adj <- generate $ elements W.adjectives
  declr <- generate $ elements ["You are", "You are as " ++ adj ++ " as"]
  return $ PP.text declr

secondPersonNonZeroConst :: IO Doc
secondPersonNonZeroConst = do
  adj <- generate $ elements W.adjectives
  declr <- generate $ elements ["You", "You are", "You are as " ++ adj ++ " as"]
  return $ PP.text declr

goTo :: IO Doc
goTo = do
  pronoun <- generate $ elements ["Let us", "We shall", "We must"]
  verb <- generate $ elements ["return to", "proceed to"]
  return $ PP.text pronoun PP.<+> PP.text verb

generatePowerOf2 :: Int -> IO Doc
generatePowerOf2 n = do
  adjs <- generate $ vectorOf (floor $ logBase 2 (abs (fromIntegral n)) :: Int) 
                              (elements W.adjectives)
  noun <- if n > 0 then generate $ elements W.positiveNouns else
                        generate $ elements W.negativeNouns
  return $ PP.text (if not (List.null adjs) &&
                       not (List.null $ head adjs) &&
                       isVowel (head $ head adjs) 
                        then "an" else "a") PP.<+> -- TODO: consider case with only noun
    PP.hsep (PP.text <$> adjs) PP.<+> PP.text noun

convertIntToBinaryExp :: Int -> Expression
convertIntToBinaryExp n = if n /= 0 then
    convertPowersToExp $ convertNonZeroIntToPowersOf2 n
  else
    Constant 0

convertNonZeroIntToPowersOf2 :: Int -> [Int]
convertNonZeroIntToPowersOf2 n = let 
  bools = convertPositiveIntToBinary (abs n)
  ints = convertBoolListToPowers bools in
    if n < 0 then List.map (*(-1)) ints else ints


convertPositiveIntToBinary :: Int -> [Bool]
convertPositiveIntToBinary n = case (n > 0) of
  True -> let ints = showIntAtBase 2 Char.intToDigit n "" in
            reverse [if c == '1' then True else False | c <- ints]
  False -> [False]

convertBoolListToPowers :: [Bool] -> [Int]
convertBoolListToPowers l = List.filter (>0)
    (convertPairs $ convertWithIndex (l, 0)) where
      convertPairs = List.map (\(b, power) -> mult b power)
      mult b power = if b then 2^power else 0
      convertWithIndex ((h:t), n) = (h, n) : convertWithIndex (t, n+1)
      convertWithIndex ([], _) = []


convertPowersToExp :: [Int] -> Expression
convertPowersToExp l = let exps = fmap Constant l in
  case l of
    [] -> Constant 0
    _ -> List.foldr1 Sum exps



{-- TODO: generate arbitrary constants that aren't powers of 2 --}

negativeCompare :: IO Doc
negativeCompare = do
  negComp <- generate $ elements W.negativeComparators
  adj <- generate $ elements W.adjectives
  result <- generate $ elements [negComp, "less " ++ adj]
  return $ PP.text (result ++ " than")

positiveCompare :: IO Doc
positiveCompare = do
  posComp <- generate $ elements W.positiveComparators
  adj <- generate $ elements W.adjectives
  result <- generate $ elements [posComp, "more " ++ adj]
  return $ PP.text (result ++ " than")

eqCompare :: IO Doc
eqCompare = do 
  adj <- generate $ elements W.adjectives
  return $ PP.text "as" PP.<+> PP.text adj PP.<+> PP.text "as"

{-- Pretty Printing functions --}

splPretty :: String -> IO ()
splPretty fileName = do
  program <- parseFile fileName
  doc <- pp program
  putStr $ PP.renderStyle (PP.Style {PP.mode = PP.PageMode, 
                                     PP.lineLength = 80,
                                     PP.ribbonsPerLine = 1.0}) doc

instance PP Character where
  pp (Character name desc) = liftM3 (\x y z -> x PP.<> y PP.<+> z) 
                                (pp $ capName name) (return PP.comma) (pp desc)

instance PP String where
  pp s = return $ PP.text s

instance PP Header where
  pp (Header t l) = liftM3 (\x y z -> x PP.<> y PP.$$ z PP.<> newline PP.<> newline)
                      (pp t) 
                      (return newline) 
                      (foldM (\d c -> liftM2 (PP.$$) (return d) (pp c)) 
                        PP.empty l)

instance PP Statement where
  pp (Enter l) = return $ commandWithCharList "Enter" l
  pp (Exeunt l) = return $ commandWithCharList "Exeunt" l
  pp (Exit name) = (\x -> PP.lbrack PP.<> PP.text "Exit" PP.<+> x PP.<> PP.rbrack PP.<> newline) <$>
                   pp (capName name)
  pp (Line name sentence) = liftM3 (\x y z -> x PP.<> PP.colon PP.$$ PP.space PP.<> y PP.<> z PP.<> newline) 
                                   (pp $ capName name) (pp sentence) (punctuation sentence)

instance PP (Statement, Annotation) where
  pp (s, Annotation "" _) = pp s
  pp (Enter _, Annotation a _) = return $ PP.lbrack PP.<> PP.text a PP.<> PP.rbrack PP.<> newline
  pp (Exeunt _, Annotation a _) = return $ PP.lbrack PP.<> PP.text a PP.<> PP.rbrack PP.<> newline
  pp (Exit _, Annotation a _) = return $ PP.lbrack PP.<> PP.text a PP.<> PP.rbrack PP.<> newline
  pp (Line name sentence, Annotation a _) = liftM2 (\n p -> n PP.<> PP.colon PP.$$ PP.space PP.<> 
                                                PP.text a PP.<> p PP.<> newline)
                                      (return $ PP.text $ capName name) (punctuation sentence)

instance PP Sentence where
  pp (IfSo s) = (\ x -> PP.text "If so," PP.<+> x) <$> pp s
  pp (IfNot s) = (\ x -> PP.text "If not," PP.<+> x) <$> pp s
  pp OutputNumber = return $ PP.text "Open your heart"
  pp OutputCharacter = return $ PP.text "Speak your mind"
  pp InputNumber = return $ PP.text "Listen to your heart"
  pp InputCharacter = return $ PP.text "Open your mind"
  pp (Declaration (Constant 0)) = liftM2 (PP.<+>) secondPersonDeclare (pp $ Constant 0)
  pp (Declaration (Constant n)) = liftM2 (\you ex -> fixConstant you ex) 
    secondPersonNonZeroConst (pp $ Constant n)
  pp (Declaration e) = liftM2 (PP.<+>) secondPersonDeclare (pp e)
  pp (Push r) = (\x -> PP.text "Remember" PP.<+> x) <$> pp r
  pp Pop = return $ PP.text "Recall your imminent death" -- TODO: randomize this
  pp (GotoScene l) = liftM2 (\x y -> x PP.<+> PP.text "Scene" PP.<+> y) goTo (pp l)
  pp (GotoAct l) = liftM2 (\x y -> x PP.<+> PP.text "Act" PP.<+> y) goTo (pp l)
  pp (Conditional c) = pp c


instance PP Label where
  pp l = return $ toRoman l

instance PP Reference where
  pp You = do
            you <- generate $ elements (W.secondPerson ++ W.secondPersonReflexive)
            return $ PP.text you
  pp Me = do
            me <- generate $ elements (W.firstPerson ++ W.firstPersonReflexive)
            return $ PP.text me
  pp (They name) = pp $ capName name

instance PP Expression where
  pp (Constant 0) = return $ PP.text "nothing"
  pp (Constant v) = if convertIntToBinaryExp v == Constant v then
      generatePowerOf2 v
    else
      pp $ convertIntToBinaryExp v
  pp (Sum e1 e2) = liftM2 (\x y -> PP.text "the sum of" PP.<+> x PP.<+> PP.text "and" PP.<+> y)
                          (pp e1)
                          (pp e2)
  pp (Difference e1 e2) = liftM2 (\x y -> PP.text "the difference between" PP.<+> x PP.<+> PP.text "and" PP.<+> y)
                            (pp e1)
                            (pp e2)
  pp (Product e1 e2) = liftM2 (\x y -> PP.text "the product of" PP.<+> x PP.<+> PP.text "and" PP.<+> y)
                            (pp e1)
                            (pp e2)
  pp (Quotient e1 e2) = liftM2 (\x y -> PP.text "the quotient between" PP.<+> x PP.<+> PP.text "and" PP.<+> y)
                            (pp e1)
                            (pp e2)
  pp (Square e) = (\x -> PP.text "the square of" PP.<+> x) <$> pp e
  pp (Cube e) = (\x -> PP.text "the cube of" PP.<+> x) <$> pp e
  pp (SquareRoot e) = (\x -> PP.text "the square root of" PP.<+> x) <$> pp e
  pp (Twice e) = (\x -> PP.text "twice" PP.<+> x) <$> pp e
  pp (Mod e1 e2) = liftM2 (\x y -> PP.text "the remainder of the quotient between" PP.<+> x PP.<+> PP.text "and" PP.<+> y)
                            (pp e1)
                            (pp e2)
  pp (Var r) = pp r

instance PP Comparison where
  pp (Comparison relationship (Var (They name)) e2) = liftM2 
    (\rel ex -> PP.text ("Is " ++ capName name) PP.<+> rel PP.<+> ex)
    (pp relationship) (pp e2)
  pp (Comparison relationship (Var You) e2) = liftM3
    (\ru rel ex -> ru PP.<+> rel PP.<+> ex)
    (areYou) (pp relationship) (pp e2)
  pp (Comparison relationship (Var Me) e2) = liftM2
    (\rel ex -> PP.text "Am I" PP.<+> rel PP.<+> ex)
    (pp relationship) (pp e2)
  pp (Comparison relationship e1 e2) = liftM3
    (\ex1 rel ex2 -> PP.text "Is" PP.<+> ex1 PP.<+> rel PP.<+> ex2)
    (pp e1) (pp relationship) (pp e2)

instance PP Relationship where
  pp Lt = negativeCompare
  pp Gt = positiveCompare
  pp Le = (\posComp -> PP.text "not" PP.<+> posComp) <$> positiveCompare
  pp Ge = (\negComp -> PP.text "not" PP.<+> negComp) <$> negativeCompare
  pp E = eqCompare
  pp Ne = (\eqComp -> PP.text "not" PP.<+> eqComp) <$> eqCompare

instance PP Block where
  pp l = foldM (\d (s, a) -> liftM2 (PP.$$) (return d) (pp (s, a)))
                PP.empty (l :: [(Statement, Annotation)])


instance PP (Label, Scene) where
  pp (label, Scene desc block) = (\blck -> printScene desc label PP.$$ blck) <$> (pp block)

instance PP (Label, Act) where
  pp (label, Act desc sceneMap) = let keys = List.sort (Map.keys sceneMap) 
                                      sceneMonads = [ pp (k, Map.findWithDefault (Scene "" []) k sceneMap) | k <- keys ] in
    (foldM (\doc sceneMonad -> liftM2 (PP.$$) (return doc) sceneMonad) (printAct desc label) sceneMonads)

instance PP Program where
  pp (Program h actMap) = let keys = List.sort (Map.keys actMap)
                              actMonads = [ pp (k, Map.findWithDefault (Act "" Map.empty) k actMap) | k <- keys ] in
    (foldM (\doc actMonad -> liftM2 (PP.$$) (return doc) actMonad) (printHeader h) actMonads)

