{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}

module AST where

import Data.Map (Map)
import qualified Data.Map.Lazy as Map

import Data.Set (Set)
import qualified Data.Set as Set


data Store = Store { variables     :: Map CName (Value, [Value])
                   , onStage       :: Set CName
                   , condition     :: Maybe Bool
                   , output        :: Maybe String
                   , awaitingInput :: Maybe (CName, InputType)
                   , act           :: Label
                   , scene         :: Label
                   } deriving (Eq, Show)

emptyState = Store Map.empty Set.empty Nothing Nothing Nothing "I" "I"

data InputType = InChar | InInt deriving (Eq, Show)

-- Character name
type CName = String
type Description = String
type Title = String
type Label = String

data Character = Character CName Description deriving (Eq, Show)

-- Lets us leave lines of the source code in the AST so we can display for
-- debugging
data Exception = DivideByZero Annotation                     |
                 UnrealAnswer Annotation                     |
                 EmptyStack Annotation Store                 |
                 AmbiguousYou Annotation Store               |
                 NotOnStage CName Annotation (Set CName)     |
                 AlreadyOnStage CName Annotation (Set CName) |
                 UndefinedCondition Annotation Store         |
                 InvalidAct Label                            |
                 InvalidScene Label deriving (Eq, Show)

type Annotation = String

data Header = Header Title [Character] deriving (Eq, Show)

data Program = Program Header (Map.Map Label Act) deriving (Eq, Show)
data Act = Act Description (Map.Map Label Scene) deriving (Eq, Show)

type Block = [(Statement, Annotation)]
data Scene = Scene Description Block deriving (Eq, Show)

-- TODO: for Enter/Exit, it must be a list of 1 or more characters. We can statically
-- enforce that with a slightly different list type.
data Statement = Enter [CName]  |
                 Exit CName     |
                 Exeunt [CName] |
                 Line CName Sentence
                 deriving (Eq, Show)

type Value = Int
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
                  Mod        Expression Expression |
                  Var Reference deriving (Eq, Show)

data Relationship = Lt | Le | E | Ne | Gt | Ge deriving (Eq, Show)

data Comparison = Comparison Relationship Expression Expression
  deriving (Eq, Show)

data Sentence = IfSo Sentence          |
                IfNot Sentence         |
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
