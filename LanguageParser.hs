{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances      #-}
    -- TODO: remove defer type errors
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -fdefer-type-errors #-}

module LanguageParser where

import           AST
import           Control.Applicative
import           Data.Char            (isAlpha, isPunctuation, isSpace)
import qualified Data.Map.Lazy        as Map
import qualified Data.Set             as Set
import           Data.Tuple
import           Data.Void
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as P
import           Text.Numeral.Roman
import qualified WordLists            as W

type Parser = P.Parsec Void String

-- Utility functions --

tillNoInclC :: Char -> Parser String
tillNoInclC c = P.many (P.noneOf [c]) <* P.string [c]

isEndPunctuation :: Char -> Bool
isEndPunctuation c = (c == '.') || (c == '?') || (c == '!')

parseUntilEndPunc :: Parser String
parseUntilEndPunc = P.takeWhileP Nothing (not . isEndPunctuation)

-- like parseUntilEndPunc but also parses the end punctuation
parseUntilEndPunc' :: Parser String
parseUntilEndPunc' = liftA2 (++)
                     parseUntilEndPunc (P.takeWhileP Nothing isEndPunctuation)

parseUntilEndBracket :: Parser String
parseUntilEndBracket = P.takeWhileP Nothing (/= ']')

-- parse one of the given strings, case-insensitively
oneOfString' :: [String] -> Parser String
oneOfString' l = P.choice ((\s -> P.try (P.string' s <*
                 P.notFollowedBy P.letterChar)) <$> l)

constP :: a -> Parser b -> Parser a
constP a p = const a <$> (p <* P.space)

oneOfCharacterNames :: Parser String
oneOfCharacterNames = oneOfString' W.characters P.<?>
                      "a valid Shakespeare character"

oneOfSecondPersonPos :: Parser String
oneOfSecondPersonPos = oneOfString' W.secondPersonPossessive

-- TODO: Next are compile time checks.
testParse file = do
                  s <- readFile file
                  case P.parse programP file s of
                    Left err -> do
                      putStrLn (P.parseErrorPretty' s err)
                      return ()
                    Right p -> do
                      print p
                      return ()

parseFile file = do
                  s <- readFile file
                  case P.parse programP file s of
                    Left err -> error (P.parseErrorPretty' s err)
                    Right p -> return p

programP :: Parser Program
programP = liftA2 Program headerP (Map.fromList <$> many actP) <* P.eof

headerP :: Parser Header
headerP = liftA2 Header (parseUntilEndPunc' <* P.space)
          (P.many characterP <* P.space)
          P.<?> "a valid header"

characterP :: Parser Character
characterP = liftA2 Character
             (oneOfCharacterNames <* P.space <* P.char ',' <* P.space)
             (parseUntilEndPunc' <* P.space)
             P.<?> "a valid Character declaration"

actP :: Parser (Label, Act)
actP = liftA3 (\lab desc mp -> (lab, Act desc mp))
       (P.string' "Act" *> P.space1 *> labelP (tillNoInclC ':') <* P.space1)
       (parseUntilEndPunc' <* P.space)
       (Map.fromList <$> many sceneP)

sceneP :: Parser (Label, Scene)
sceneP = liftA3 (\lab desc list -> (lab, Scene desc list))
         (P.string' "Scene" *> P.space1 *> labelP (tillNoInclC ':') <* P.space1)
         (parseUntilEndPunc' <* P.space)
         (concat <$> many (P.try listOfStatementP))

labelP :: Parser String -> Parser Label
labelP stringP = do
  s <- stringP
  case fromRoman s of
    Just n -> return n
    Nothing -> P.fancyFailure (Set.singleton (P.ErrorFail
               (s ++ " is not a valid roman numeral")))

listOfStatementP :: Parser [(Statement, Annotation)]
listOfStatementP = P.try ((:[]) <$> enterP) <|>
                   P.try ((:[]) <$> exitP) <|>
                   P.try ((:[]) <$> exeuntP) <|>
                   lineP

enterExitAnnotationP :: Parser Annotation
enterExitAnnotationP = P.try (P.lookAhead (P.char '[' *> parseUntilEndBracket))

enterP :: Parser (Statement, Annotation)
enterP = swap <$> liftA2 (,) enterExitAnnotationP enterP'
         where
            enterP' = Enter <$> (P.char '[' *> P.space *> P.string' "Enter" *>
                      P.space1 *> (P.try double <|> single) <* P.space <*
                      P.char ']' <* P.space)

exitP :: Parser (Statement, Annotation)
exitP = swap <$> liftA2 (,) enterExitAnnotationP exitP'
         where
           exitP' = Exit <$> (P.char '[' *> P.space *> P.string' "Exit" *>
                    P.space1 *> oneOfCharacterNames <* P.space <*
                    P.char ']' <* P.space)

exeuntP :: Parser (Statement, Annotation)
exeuntP = swap <$> liftA2 (,) enterExitAnnotationP exeuntP'
           where
             exeuntP' = Exeunt <$> (P.char '[' *> P.space *> P.string'
                        "Exeunt" *> (P.try (P.space1 *> double) <|> none) <*
                        P.space <* P.char ']' <* P.space)

none :: Parser [String]
none = const [] <$> P.takeP Nothing 0

single :: Parser [String]
single = (:[]) <$> oneOfCharacterNames <* P.space

-- TODO: Handle three, four, five, etc [though not necessary].
double :: Parser [String]
double = liftA2 (\a b -> [a, b])
         (oneOfCharacterNames <* P.space1 <* P.string "and" <* P.space1)
         (oneOfCharacterNames <* P.space)

lineP :: Parser [(Statement, Annotation)]
lineP = liftA2 combine (Line <$> oneOfCharacterNames <* P.char ':' <* P.space)
        listOfSentenceP <* P.space
        where
          combine line senAnnList = (\(s, a) -> (line s, a)) <$> senAnnList

listOfSentenceP :: Parser [(Sentence, Annotation)]
listOfSentenceP = some sentenceP

ifSoP :: Parser Sentence
ifSoP = IfSo <$> (P.string' "If" *> P.space1 *> P.string' "so" *>
                 P.space *> P.char ',' *> P.space *> sentenceP')

ifNotP :: Parser Sentence
ifNotP = IfNot <$> (P.string' "If" *> P.space1 *> P.string' "not" *>
                   P.space *> P.char ',' *> P.space *> sentenceP')

outputNumberP :: Parser Sentence
outputNumberP = constP OutputNumber
                (P.string' "Open" <* P.space1 <* oneOfSecondPersonPos <*
                P.space1 <* P.string' "heart")

outputCharacterP :: Parser Sentence
outputCharacterP = constP OutputCharacter
                   (P.string' "Speak" <* P.space1 <* oneOfSecondPersonPos <*
                   P.space1 <* P.string' "mind")

inputNumberP :: Parser Sentence
inputNumberP = constP InputNumber
               (P.string' "Listen" <* P.space1 <* P.string "to" <* P.space1 <*
               oneOfSecondPersonPos <* P.space1 <* P.string' "heart")

inputCharacterP :: Parser Sentence
inputCharacterP = constP InputCharacter
                  (P.string' "Open" <* P.space1 <* oneOfSecondPersonPos <*
                  P.space1 <* P.string' "mind")

declarationP :: Parser Sentence
declarationP =
  P.try decVarient1 <|> P.try decVarient2 <|> decVarient3
  where

    decVarient1 :: Parser Sentence
    decVarient1 = Declaration <$> (oneOfString' W.secondPerson *> P.space1 *>
                                  oneOfString' W.be *> P.space1 *>
                                  P.string' "as" *> P.space1 *>
                                  oneOfString' W.adjectives *> P.space1 *>
                                  P.string' "as" *> P.space1 *> expressionP)

    decVarient2 :: Parser Sentence
    decVarient2 = Declaration <$> (oneOfString' W.secondPerson *> P.space1 *>
                                  oneOfString' W.be *> P.space1 *> expressionP)

    decVarient3 :: Parser Sentence
    decVarient3 = Declaration <$> (oneOfString' W.secondPerson *> P.space1 *>
                                  expressionP)

pushP :: Parser Sentence
pushP = Push <$> (P.string' "Remember" *> P.space1 *> referenceP)

popP :: Parser Sentence
popP = constP Pop (P.string' "Recall") <* parseUntilEndPunc

genericGoTo :: (Label -> Sentence) -> String -> Parser Sentence
genericGoTo con s = con <$> (oneOfString' ["Let us", "We shall", "We must"] *>
                    P.space1 *> oneOfString' ["return to", "proceed to"] *>
                    P.space1 *> P.string' s *> P.space1 *>
                    labelP parseUntilEndPunc)

goToSceneP :: Parser Sentence
goToSceneP = genericGoTo GotoScene "scene"

goToActP :: Parser Sentence
goToActP = genericGoTo GotoAct "act"

-- TODO: this does NOT enforce an ending of ? right now. I think this is fine, let me know what you think.
conditionalP :: Parser Sentence
conditionalP = Conditional <$> comparisonP

sentenceP' :: Parser Sentence
sentenceP' = P.try ifSoP <|>
             P.try ifNotP <|>
             P.try outputNumberP <|>
             P.try outputCharacterP <|>
             P.try inputNumberP <|>
             P.try inputCharacterP <|>
             P.try declarationP <|>
             P.try pushP <|>
             P.try popP <|>
             P.try goToSceneP <|>
             P.try goToActP <|>
             conditionalP
             P.<?> "a valid sentence"

sentenceP :: Parser (Sentence, Annotation)
-- TODO: will punctuationChar pass on comma? Is that cool?
sentenceP = swap <$> liftA2 (,) (P.try (P.lookAhead parseUntilEndPunc))
            (sentenceP' <* P.space <* P.punctuationChar <* P.space)

negativeComparatorP :: Parser String
negativeComparatorP =
  P.try (P.string' "less" *> P.space1 *> oneOfString' W.adjectives) <|>
  oneOfString' W.negativeComparators

positiveComparatorP :: Parser String
positiveComparatorP =
  P.try (P.string' "more" *> P.space1 *> oneOfString' W.adjectives) <|>
  oneOfString' W.positiveComparators

comparisonP :: Parser Comparison
comparisonP = P.try equalsP <|>
              P.try lessThanP <|>
              P.try lessThanEqualsP <|>
              P.try notEqualsP <|>
              P.try greaterThanP <|>
              greaterThanEqualsP

              where
                genericComparison :: Relationship -> Parser a ->
                                     Parser Comparison
                genericComparison rel customP =
                  liftA2 (Comparison rel)
                  (oneOfString' W.be *> P.space1 *> expressionP <* customP)
                  expressionP

                equalsP :: Parser Comparison
                equalsP = genericComparison E
                          (P.string' "as" <* P.space1 <*
                          oneOfString' W.adjectives <* P.space1 <*
                          P.string' "as" <* P.space1)

                lessThanP :: Parser Comparison
                lessThanP = genericComparison Lt
                            (negativeComparatorP <* P.space1 <* P.string' "than"
                            <* P.space1)

                lessThanEqualsP :: Parser Comparison
                lessThanEqualsP = genericComparison Le
                                  (P.string "not" <* P.space1 <*
                                  positiveComparatorP <* P.space1 <*
                                  P.string' "than" <* P.space1)

                notEqualsP :: Parser Comparison
                notEqualsP = genericComparison Ne
                             (P.string "not" <* P.space1 <* P.string' "as" <*
                             P.space1 <* oneOfString' W.adjectives <*
                             P.space1 <* P.string' "as" <* P.space1)

                greaterThanP :: Parser Comparison
                greaterThanP = genericComparison Gt
                               (positiveComparatorP <* P.space1 <*
                               P.string' "than" <* P.space1)

                greaterThanEqualsP :: Parser Comparison
                greaterThanEqualsP = genericComparison Ge
                                     (P.string "not" <* P.space1 <*
                                     negativeComparatorP <* P.space1 <*
                                     P.string' "than" <* P.space1)

expressionP :: Parser Expression
expressionP = P.try varP <|>
              P.try constantP <|>
              P.try sumP <|>
              P.try productP <|>
              P.try differenceP <|>
              P.try quotientP <|>
              P.try squareP <|>
              P.try cubeP <|>
              P.try squareRootP <|>
              P.try twiceP <|>
              P.try modP

constantP :: Parser Expression
constantP = P.try (constP (Constant 0) (P.string' "nothing" <* P.space)) <|>
            P.try (genericConstant (2 ^) (W.positiveNouns ++ W.neutralNouns)) <|>
            genericConstant (negate . (2 ^)) W.negativeNouns
            where
              genericConstant f l =
                Constant . f . length <$>
                (P.try (consBeginning *> rest l) <|> rest l)

              rest l = P.space *> many (oneOfString' W.adjectives <* P.space1)
                       <* P.space <* possesives <* oneOfString' l <* P.space

              possesives = optional (oneOfString' W.possesives) <* P.space

              consBeginning =
                oneOfString' (W.articles ++ W.firstPersonPossessive ++
                             W.secondPersonPossessive ++ W.thirdPersonPossessive)

              empty = const "" <$> P.takeP Nothing 0

binOp :: (Expression -> Expression -> Expression) -> String -> String ->
         Parser Expression
binOp con word prep =
  liftA2 con
  (P.string' "the" *> P.space1 *> P.string' word *> P.space1 *> P.string' prep
  *> P.space *> expressionP) (P.string' "and" *> P.space1 *> expressionP)

sumP :: Parser Expression
sumP = binOp Sum "sum" "of"

differenceP :: Parser Expression
differenceP = binOp Difference "difference" "between"

productP :: Parser Expression
productP = binOp Product "product" "of"

quotientP :: Parser Expression
quotientP = binOp Quotient "quotient" "between"

unOp :: (Expression -> Expression) -> Parser a -> Parser Expression
unOp con word = con <$> (word *> P.space *> expressionP)

squareP :: Parser Expression
squareP = unOp Square (P.string' "the" <* P.space1 <* P.string' "square" <*
                      P.space1 <* P.string' "of")

cubeP :: Parser Expression
cubeP = unOp Cube (P.string' "the" <* P.space1 <* P.string' "cube" <*
                  P.space1 <* P.string' "of")

squareRootP :: Parser Expression
squareRootP = unOp SquareRoot (P.string' "the" <* P.space1 <* P.string' "square"
                              <* P.space1 <* P.string' "root" <* P.space1 <*
                              P.string' "of")

twiceP :: Parser Expression
twiceP = unOp Twice (P.string' "twice")

modP :: Parser Expression
modP = liftA2 Mod
       (P.string' "the" *> P.space1 *> P.string' "remainder" *> P.space1 *>
       P.string' "of" *> P.space1 *> P.string' "the" *> P.space1 *>
       P.string' "quotient" *> P.space1 *> P.string' "between" *> P.space1 *>
       expressionP)
       (P.string' "and" *> P.space1 *> expressionP)

varP :: Parser Expression
varP = Var <$> referenceP <* P.space

referenceP :: Parser Reference
referenceP = (P.try youP <|> P.try meP <|> theyP) <* P.space
             where
               youP = constP You (oneOfString' (W.secondPersonReflexive ++
                                                W.secondPerson))
               meP = constP Me (oneOfString' (W.firstPersonReflexive ++
                                              W.firstPerson))
               theyP = They <$> oneOfString' W.characters
