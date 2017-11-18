module LanguageParser where

import qualified Parser as P
import qualified ParserCombinators as P

programP :: P.Parser Program
programP = undefined

headerP :: P.Parser Header
headerP = undefined

actP :: P.Parser (Label, Act)
actP = undefined

sceneP :: P.Parser (Label, Scene)
sceneP = undefined

statementP :: P.Parser Statement
statementP = undefined

expressionP :: P.Parser Expression
expressionP = undefined

valueP :: P.Parser Value
valueP = undefined

relationshipP :: P.Parser Relationship
relationshipP = undefined

comparisonP :: P.Parser Comparison
comparisonP = undefined

sentenceP :: P.Parser Sentence
sentenceP = undefined
