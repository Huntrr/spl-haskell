import qualified Data.Map.Lazy as Map

type Title = String

-- Character name
type CName = String

type Description = String

data Character = Character CName Description deriving (Eq, Show)

data Header = Header Title [Character] deriving (Eq, Show)

data Program = Program Header (Map.Map String Act) deriving (Eq, Show)

data Act = Act Description (Map.Map String Scene) deriving (Eq, Show)

data Scene = Scene Description [Statement] deriving (Eq, Show)

-- TODO: for Enter, it must be a list of 1 or more characters. We can statically
-- enforce that with a slightly different list type.
-- TODO: for Exeunt, the list length must be 0 or 2.
-- TODO: for Line, the list length must be 1 or more.
-- TODO: the spec actually uses Statement to refer to a declaration. Should we
-- change this to match that?
data Statement = Enter [CName] |
                 Exit CName |
                 Exeunt [CName] |
                 Line CName [Sentence]
                 deriving (Eq, Show)

-- TODO: should Value retain the underlying structure (e.g. what is the noun)
-- , how many adjectives, etc?
-- TODO: Make Value actually include references to characters and constants
type Value = Int

data Relationship = Lt | Le | E | Gt | Ge deriving (Eq, Show)

data Comparison = Comparison Value Relationship Value deriving (Eq, Show)

-- TODO: actually, declarations/push/pop don't need a CName right?
data Sentence = IfSo Sentence |
                OutputNumber |
                OutputCharacter |
                InputNumber |
                InputCharacter |
                Declaration CName Value |
                Push CName Value |
                Pop CName Value |
                GotoScene String |
                GotoAct String |
                Conditional Comparison
                deriving (Eq, Show)
