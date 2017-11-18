module AST where

import qualified Data.Map.Lazy as Map

-- Character name
type Character = String

type Label = String

newtype Header = Header [Character] deriving (Eq, Show)

data Program = Program Header (Map.Map Label Act) deriving (Eq, Show)
data Act = Act Description (Map.Map Label Scene) deriving (Eq, Show)

newtype Scene = Scene [Statement] deriving (Eq, Show)

-- TODO: for Enter, it must be a list of 1 or more characters. We can statically
-- enforce that with a slightly different list type.
-- TODO: for Exeunt, the list length must be 0 or 2.
-- TODO: for Line, the list length must be 1 or more.
-- TODO: the spec actually uses Statement to refer to a declaration. Should we
-- change this to match that?
data Statement = Enter [Character] |
                 Exit  [Character] |
                 Exeunt            |
                 Line Character [Sentence]
                 deriving (Eq, Show)

-- TODO: should Value retain the underlying structure (e.g. what is the noun)
-- , how many adjectives, etc?
-- TODO: Make Value actually include references to characters and constants
type Value = Int

data Expression = Constant Value                   |
                  Sum        Expression Expression |
                  Difference Expression Expression |
                  Square     Expression Expression |
                  Cube       Expression Expression |
                  SquareRoot Expression Expression |
                  Var Variable

data Relationship = Lt | Le | E | Gt | Ge deriving (Eq, Show)

data Comparison = Comparison Expression Relationship Expression
  deriving (Eq, Show)

-- TODO: actually, declarations/push/pop don't need a CName right?
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
