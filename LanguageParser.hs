{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction,
    FlexibleInstances #-}
    -- TODO: remove defer type errors
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -fdefer-type-errors #-}

module LanguageParser where

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Data.Map.Lazy as Map
import Data.Char (isSpace, isAlpha)
import Control.Applicative
import Data.Void

import AST

type Parser = P.Parsec Void String

-- Like manyTill but includes c
tillInclC :: Char -> Parser String
tillInclC c = liftA2 (++) (P.many (P.noneOf [c])) (P.string [c])

-- TODO: use endBy?
tillNoInclC :: Char -> Parser String
tillNoInclC c = (P.many (P.noneOf [c])) <* (P.string [c])

-- TODO: characters are case insensitive for sure, see "the ghost" in primes
oneOfString' :: [String] -> Parser String
oneOfString' l = P.choice (P.string' <$> l)

programP :: Parser Program
programP = undefined

headerP :: Parser Header
headerP = liftA2 Header (tillInclC '.' <* P.space) (P.many characterP <* P.space)

-- TODO: this will parse 'Ben' as a character even though that's not valid. Should we
-- enforce that in the parser?
characterP :: Parser Character
characterP = liftA2 Character (tillNoInclC ',' <* P.space) (tillInclC '.' <* P.space)

actP :: Parser Act
actP = undefined
       where
         mapP :: Parser (Map.Map Label Scene)
         mapP = Map.fromList <$> many tupleP

         -- TODO: think this needs to parse the title?
         tupleP :: Parser (Label, Scene)
         tupleP = liftA2 (,) (P.string "Scene " *> tillNoInclC ':' <* P.space) sceneP

sceneP :: Parser Scene
sceneP = undefined

statementP :: Parser Statement
statementP = P.try enterP <|>
             P.try exitP <|>
             P.try exeuntP <|>
             lineP

enterP :: Parser Statement
-- TODO: string' is case insensitive
-- enterP = P.between (char '[') (sepBy1 P.anyChar P.space *> P.string "and" <* P.space) (char ']')
enterP = Enter <$> (P.char '[' *> P.space *> P.string' "Enter" *> P.space1 *> (P.try double <|> single) <* P.space <* (P.char ']'))

exitP :: Parser Statement
exitP = Exit <$> (P.char '[' *> P.space *> P.string' "Exit" *> P.space1 *> (some P.letterChar) <* P.space <* (P.char ']'))

exeuntP :: Parser Statement
exeuntP = Exeunt <$> (P.char '[' *> P.space *> P.string' "Exeunt" *> (P.try (P.space1 *> double) <|> none) <* P.space <* (P.char ']'))

none :: Parser [String]
none = const [] <$> (P.takeP Nothing 0)

single :: Parser [String]
single = (:[]) <$> (some P.letterChar) <* P.space

double :: Parser [String]
double = (liftA2 (\a b -> [a, b]) ((some P.letterChar) <* P.space1 <* P.string "and" <* P.space1) ((some P.letterChar) <* P.space))

lineP :: Parser Statement
lineP = undefined

-- TODO: just for testing
positiveNouns = ["Flower", "Tree", "pony", "hound"]
negativeNouns = ["Pig", "codpiece"]
adjectives = ["amazing", "little", "big", "hairy", "sorry", "little"]

expressionP :: Parser Expression
expressionP = P.try constantP <|>
              P.try sumP <|>
              P.try productP <|>
              P.try differenceP <|>
              P.try squareP <|>
              P.try cubeP <|>
              P.try squareRootP <|>
              P.try twiceP <|>
              varP

constantP :: Parser Expression
constantP = P.try (genericConstant (2 ^) positiveNouns) <|> genericConstant (negate . (2 ^)) negativeNouns
            where
              -- TODO: should we require there be at least 1 space?
              genericConstant f l = Constant <$> f . length <$> (consBeginning *> P.space *> many ((oneOfString' adjectives) <* P.space) <* P.space <* oneOfString' l <* P.space)

              -- also case insensitive
              consBeginning = oneOfString' ["my", "your", "an", "a"]

-- TODO: right now, the "sum" of and "and" are case insensitive in case a sentence starts with "The". Does that make sense?
binOp :: (Expression -> Expression -> Expression) -> String -> String -> Parser Expression
binOp con word prep = liftA2 con (P.string' ("the " ++ word ++ " " ++ prep) *> P.space *> expressionP) (P.string' "and" *> P.space *> expressionP)

sumP :: Parser Expression
sumP = binOp Sum "sum" "of"

differenceP :: Parser Expression
differenceP = binOp Difference "difference" "between"

productP :: Parser Expression
productP = binOp Product "product" "of"

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
varP = Var <$> (some (P.satisfy (\c -> c /= '.' && (not . isSpace) c)) <* P.space)
-- varP = Var <$> ((some P.anyChar) <* P.space)
-- TODO: punctuation is incomplete
-- varP = Var <$> P.manyTill P.anyChar (P.lookAhead (P.space *> (oneOfString' ["and", ".", "?", "!"])))
