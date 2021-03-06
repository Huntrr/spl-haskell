{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction,
    FlexibleInstances #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}

module LanguageParser where

import qualified Parser as P
import qualified ParserCombinators as P

import AST

programP :: P.Parser Program
programP = undefined

headerP :: P.Parser Header
headerP = undefined

actP :: P.Parser (Label, Act)
actP = undefined

sceneP :: P.Parser (Label, Scene)
sceneP = undefined

statementP :: P.Parser (Statement, Annotation)
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
