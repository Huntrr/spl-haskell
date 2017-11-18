module Vocab where

import Test.QuickCheck (Gen(..))

character :: Gen Character
character = elements ["Romeo", "Juliet"]

-- TODO: etc (probably reading from files!)
