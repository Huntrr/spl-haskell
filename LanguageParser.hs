{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances      #-}
    -- TODO: remove defer type errors
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -fdefer-type-errors #-}

module LanguageParser where

import           Control.Applicative
import           Data.Char            (isAlpha, isPunctuation, isSpace)
import qualified Data.Map.Lazy        as Map
import           Data.Tuple
import           Data.Void
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as P
import qualified WordLists            as W

import           AST

type Parser = P.Parsec Void String

-- TODO: Parse 'nothing' to make primes.spl pass?

-- Like manyTill but includes c
tillInclC :: Char -> Parser String
tillInclC c = liftA2 (++) (P.many (P.noneOf [c])) (P.string [c])

-- TODO: use endBy?
tillNoInclC :: Char -> Parser String
tillNoInclC c = P.many (P.noneOf [c]) <* P.string [c]

-- TODO: characters are case insensitive for sure, see "the ghost" in primes
oneOfString' :: [String] -> Parser String
oneOfString' l = P.choice ((\s -> P.try (P.string' s <*
                 P.notFollowedBy P.letterChar)) <$> l)

constP :: a -> Parser b -> Parser a
constP a p = const a <$> (p <* P.space)

oneOfCharacterNames :: Parser String
oneOfCharacterNames = oneOfString' W.characters P.<?>
                      "Expecting a valid Shakespeare character"

oneOfSecondPersonPos :: Parser String
oneOfSecondPersonPos = oneOfString' W.secondPersonPossessive

characterP :: Parser Character
characterP = liftA2 Character
             (oneOfCharacterNames <* P.space <* P.char ',' <* P.space)
             (tillInclC '.' <* P.space)
             P.<?> "Not a valid Character declaration"

-- TODO: Replace with something more real.
testParse file = P.runParser programP file <$> readFile file

programP :: Parser Program
programP = liftA2 Program headerP (Map.fromList <$> many actP) <* P.eof

headerP :: Parser Header
headerP = liftA2 Header (tillInclC '.' <* P.space)
          (P.many characterP <* P.space)
          P.<?> "Not a valid header"

actP :: Parser (Label, Act)
actP = liftA3 (\lab desc mp -> (lab, Act desc mp))
       (P.string' "Act" *> P.space1 *> tillNoInclC ':' <* P.space1)
       (tillInclC '.' <* P.space)
       (Map.fromList <$> many sceneP)

sceneP :: Parser (Label, Scene)
-- TODO: case insensitive!
-- TODO: the P.try for listOfStatementP is necessary
sceneP = liftA3 (\lab desc list -> (lab, Scene desc list))
         (P.string' "Scene" *> P.space1 *> tillNoInclC ':' <* P.space1)
         (tillInclC '.' <* P.space)
         (concat <$> many (P.try listOfStatementP))

listOfStatementP :: Parser [(Statement, Annotation)]
listOfStatementP = P.try ((:[]) <$> enterP) <|>
                   P.try ((:[]) <$> exitP) <|>
                   P.try ((:[]) <$> exeuntP) <|>
                   lineP

enterExitAnnotationP :: Parser Annotation
enterExitAnnotationP = P.try (P.lookAhead (P.char '[' *> parseNoEndBracket))

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

-- TODO: Need to handle three, four, five, etc.
double :: Parser [String]
double = liftA2 (\a b -> [a, b]) (oneOfCharacterNames <* P.space1 <* P.string "and" <* P.space1) (oneOfCharacterNames <* P.space)

lineP :: Parser [(Statement, Annotation)]
lineP = liftA2 combine (Line <$> oneOfCharacterNames <* P.char ':' <* P.space)
        listOfSentenceP <* P.space
        where
          combine line senAnnList = (\(s, a) -> (line s, a)) <$> senAnnList

listOfSentenceP :: Parser [(Sentence, Annotation)]
-- TODO: this is this way because the last one will fail because it will be
-- either Character: or Act I: so try pretends like it never parsed.
-- listOfSentenceP = some (P.try sentenceP)
-- TODO: is the above true anymore ^? I don't think so.
listOfSentenceP = some sentenceP

parseNoPunc :: Parser String
parseNoPunc = P.takeWhileP Nothing (not . isPunctuation)

parseNoEndBracket :: Parser String
parseNoEndBracket = P.takeWhileP Nothing (/= ']')

-- TODO: Again, all is case insensitive.
ifSoP :: Parser Sentence
ifSoP = IfSo <$> (P.string' "If so" *> P.space *> P.char ',' *> P.space *> sentenceP')

outputNumberP :: Parser Sentence
outputNumberP = constP OutputNumber (P.string' "Open" <* P.space1 <* oneOfSecondPersonPos <* P.space1 <* P.string' "heart")

outputCharacterP :: Parser Sentence
outputCharacterP = constP OutputCharacter (P.string' "Speak" <* P.space1 <* oneOfSecondPersonPos <* P.space1 <* P.string' "mind")

inputNumberP :: Parser Sentence
inputNumberP = constP InputNumber (P.string' "Listen" <* P.space1 <* P.string "to" <* P.space1 <* oneOfSecondPersonPos <* P.space1 <* P.string' "heart")

inputCharacterP :: Parser Sentence
inputCharacterP = constP InputCharacter (P.string' "Open" <* P.space1 <* oneOfSecondPersonPos <* P.space1 <* P.string' "mind")

declarationP :: Parser Sentence
declarationP = P.try decVarient1 <|> decVarient2
               where
                 decVarient1 = Declaration <$> (oneOfString' W.secondPerson *> P.space1 *> oneOfString' W.be *> P.space1 *> P.string' "as" *> P.space1 *> oneOfString' W.adjectives *> P.space1 *> P.string' "as" *> P.space1 *> expressionP)
                 decVarient2 = Declaration <$> (oneOfString' W.secondPerson *> P.space1 *> expressionP)
pushP :: Parser Sentence
pushP = constP Push (P.string' "Remember me")

-- TODO same punctuation issue as maybe above
-- TODO: refactor not is punc into own function
popP :: Parser Sentence
popP = (constP Pop (P.string' "Recall")) <* P.takeWhileP Nothing (not . isPunctuation)

genericGoTo :: (String -> Sentence) -> String -> Parser Sentence
genericGoTo con s = con <$> ((oneOfString' ["Let us", "We shall", "We must"]) *>
                    P.space1 *> (oneOfString' ["return to", "proceed to"]) *>
                    P.space1 *> P.string' s *> P.space1 *> P.takeWhileP Nothing (not . isPunctuation))

goToSceneP :: Parser Sentence
goToSceneP = genericGoTo GotoScene "scene"

goToActP :: Parser Sentence
goToActP = genericGoTo GotoAct "act"

-- TODO: this does NOT enforce an ending of ? right now. I think this is fine, let me know what you think.
conditionalP :: Parser Sentence
conditionalP = Conditional <$> comparisonP

sentenceP' :: Parser Sentence
sentenceP' = P.try ifSoP <|>
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
             P.<?> "Excpecting a valid sentence"

sentenceP :: Parser (Sentence, Annotation)
-- TODO: will punctuationChar pass on comma? Is that cool?
sentenceP = swap <$> liftA2 (,) (P.try (P.lookAhead parseNoPunc))
            (sentenceP' <* P.space <* P.punctuationChar <* P.space)

comparisonP :: Parser Comparison
comparisonP = P.try equalsP <|>
              P.try lessThanP <|>
              P.try lessThanEqualsP <|>
              P.try notEqualsP <|>
              P.try greaterThanP <|>
              greaterThanEqualsP

              where
                genericComparison :: Relationship -> Parser a -> Parser Comparison
                genericComparison rel customP = liftA2 (Comparison rel)
                                                (oneOfString' W.be *> P.space1 *> expressionP <* customP)
                                                expressionP

                equalsP :: Parser Comparison
                equalsP = genericComparison E (P.string' "as" <* P.space1 <* oneOfString' W.positiveAdjectives <* P.space1 <* P.string' "as" <* P.space1)

                lessThanP :: Parser Comparison
                lessThanP = genericComparison Lt (oneOfString' W.negativeComparators <* P.space1 <* P.string' "than" <* P.space1)

                lessThanEqualsP :: Parser Comparison
                lessThanEqualsP = genericComparison Le (P.string "not" <* P.space1 <* oneOfString' W.positiveComparators <* P.space1 <* P.string' "than" <* P.space1)

                notEqualsP :: Parser Comparison
                notEqualsP = genericComparison Ne (P.string "not" <* P.space1 <* P.string' "as" <* P.space1 <* oneOfString' W.positiveAdjectives <* P.space1 <* P.string' "as" <* P.space1)

                greaterThanP :: Parser Comparison
                greaterThanP = genericComparison Gt (oneOfString' W.positiveComparators <* P.space1 <* P.string' "than" <* P.space1)

                greaterThanEqualsP :: Parser Comparison
                greaterThanEqualsP = genericComparison Ge (P.string "not" <* P.space1 <* oneOfString' W.negativeComparators <* P.space1 <* P.string' "than" <* P.space1)

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
              P.try twiceP

constantP :: Parser Expression
constantP = P.try (genericConstant (2 ^) (W.positiveNouns ++ W.neutralNouns)) <|> genericConstant (negate . (2 ^)) W.negativeNouns
            where
              -- TODO: should we require there be at least 1 space?
              genericConstant f l = Constant <$> f . length <$> (P.try (consBeginning *> rest l) <|> rest l)

              rest l = P.space *> many ((oneOfString' W.adjectives) <* P.space) <* P.space <* oneOfString' l <* P.space

              -- also case insensitive
              -- TODO: is this the right concatenation
              consBeginning = oneOfString' (W.articles ++ W.firstPersonPossessive ++ W.secondPersonPossessive ++ W.thirdPersonPossessive)

              empty = const "" <$> (P.takeP Nothing 0)

-- TODO: right now, the "sum" of and "and" are case insensitive in case a sentence starts with "The". Does that make sense?
binOp :: (Expression -> Expression -> Expression) -> String -> String -> Parser Expression
binOp con word prep = liftA2 con (P.string' "the" *> P.space1 *> P.string' word *> P.space1 *> P.string' prep *> P.space *> expressionP) (P.string' "and" *> P.space *> expressionP)

sumP :: Parser Expression
sumP = binOp Sum "sum" "of"

-- TODO: this actually had a bug because the original parser didn't support a \n
-- between difference and between. A good way to test this whole parser is on a
-- version of hello.spl where all spaces are changed to newlines (or lots of spaces).
differenceP :: Parser Expression
differenceP = binOp Difference "difference" "between"

productP :: Parser Expression
productP = binOp Product "product" "of"

quotientP :: Parser Expression
quotientP = binOp Quotient "quotient" "between"

unOp :: (Expression -> Expression) -> String -> Parser Expression
unOp con word = con <$> (P.string' word *> P.space *> expressionP)

-- square of ___
-- TODO: like above, this is case insensitive
-- All below too.
squareP :: Parser Expression
squareP = unOp Square "the square of"

-- cube of ___
cubeP :: Parser Expression
cubeP = unOp Cube "the cube of"

-- square root of ___
squareRootP :: Parser Expression
squareRootP = unOp SquareRoot "the square root of"

-- twice ___
twiceP :: Parser Expression
twiceP = unOp Twice "twice"

-- TODO: doesn't work for multi-word characters yet
varP :: Parser Expression
-- TODO: second person because of comparisons "Am I better than you?"
varP = Var <$> referenceP <* P.space
-- varP = Var <$> ((some P.anyChar) <* P.space)
-- TODO: punctuation is incomplete
-- varP = Var <$> P.manyTill P.anyChar (P.lookAhead (P.space *> (oneOfString' ["and", ".", "?", "!"])))

referenceP :: Parser Reference
referenceP = (P.try youP <|> P.try meP <|> theyP) <* P.space
             where
               youP = constP You (oneOfString' (W.secondPersonReflexive ++ W.secondPerson))
               meP = constP Me (oneOfString' (W.firstPersonReflexive ++ W.firstPerson))
               theyP = They <$> oneOfString' W.characters
