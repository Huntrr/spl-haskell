{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction,
    FlexibleInstances #-}
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
type Annotation = (String, Int)
data Exception = DivideByZero |
                 EmptyStack deriving (Eq, Show)

data Header = Header Title [Character] deriving (Eq, Show)

data Program = Program Header (Map.Map Label Act) deriving (Eq, Show)
data Act = Act Description (Map.Map Label Scene) deriving (Eq, Show)

newtype Scene = Scene [(Statement, Annotation)] deriving (Eq, Show)

-- TODO: for Enter/Exit, it must be a list of 1 or more characters. We can statically
-- enforce that with a slightly different list type.
data Statement = Enter [CName] |
                 Exit  [CName] |
                 Exeunt        |
                 Line Character Sentence
                 deriving (Eq, Show)

type Value = Int

data Expression = Constant Value                   |
                  Sum        Expression Expression |
                  Difference Expression Expression |
                  Square     Expression            |
                  Cube       Expression            |
                  SquareRoot Expression            |
                  Twice      Expression            |
                  Var Character deriving (Eq, Show)

data Relationship = Lt | Le | E | Ne | Gt | Ge deriving (Eq, Show)

data Comparison = Comparison Expression Relationship Expression
  deriving (Eq, Show)

data Sentence = IfSo Sentence          |
                OutputNumber           |
                OutputCharacter        |
                InputNumber            |
                InputCharacter         |
                Declaration Expression |
                Push                   |
                Pop                    |
                GotoScene Label        |
                GotoAct Label          |
                Conditional Comparison
                deriving (Eq, Show)
