module Evaluator where

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad ()
import Control.Monad.State (State)
import qualified Control.Monad.State as S

-- TODO: Store will probably change
data Store = Store { getCharMap     :: Map Character Value
                   , getActiveChars :: [Character]
                   } deriving (Eq, Show)

evalE :: Expression -> State Store Value
evalE = undefined

-- TODO: Need to figure out which monad types/style we're using before
--       I can stub out the rest of these definitions
