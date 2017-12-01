{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}

module AST where

import qualified Data.Map.Lazy as Map

-- Character name
type CName = String
type Description = String
type Title = String
type Label = String

data Character = Character CName Description deriving (Eq, Show)

-- Lets us leave lines of the source code in the AST so we can display for
-- debugging
-- TODO: change to include line number/character position/other data
type Annotation = String
data Exception = DivideByZero |
                 EmptyStack deriving (Eq, Show)

data Header = Header Title [Character] deriving (Eq, Show)

data Program = Program Header (Map.Map Label Act) deriving (Eq, Show)
data Act = Act Description (Map.Map Label Scene) deriving (Eq, Show)

data Scene = Scene Description [(Statement, Annotation)] deriving (Eq, Show)

-- TODO: for Enter/Exit, it must be a list of 1 or more characters. We can statically
-- enforce that with a slightly different list type.
data Statement = Enter [CName]  |
                 Exit CName     |
                 Exeunt [CName] |
                 Line CName Sentence
                 deriving (Eq, Show)

type Value = Int
-- TODO: I am using Reference instead of CName because variables can be, and often
-- are second person pronouns. They can also just be regular character names.
data Reference = They CName | You | Me deriving (Eq, Show)

data Expression = Constant Value                   |
                  Sum        Expression Expression |
                  Difference Expression Expression |
                  Product    Expression Expression |
                  Quotient   Expression Expression |
                  Square     Expression            |
                  Cube       Expression            |
                  SquareRoot Expression            |
                  Twice      Expression            |
                  Var Reference deriving (Eq, Show)

data Relationship = Lt | Le | E | Ne | Gt | Ge deriving (Eq, Show)

data Comparison = Comparison Relationship Expression Expression
  deriving (Eq, Show)

data Sentence = IfSo Sentence          |
                OutputNumber           |
                OutputCharacter        |
                InputNumber            |
                InputCharacter         |
                Declaration Expression |
                Push Reference         |
                Pop                    |
                GotoScene Label        |
                GotoAct Label          |
                Conditional Comparison
                deriving (Eq, Show)
