{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction,
    FlexibleInstances #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}

module Evaluator where

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.State (MonadState(..), StateT, State, runState, runStateT)
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)

import AST

-- TODO: Store will probably change
data Store = Store { getCharMap     :: Map Character Value
                   , getActiveChars :: [Character]
                   , getCondition   :: Bool
                   , getSpeaker     :: Maybe Character
                   , getProgMap     :: Map Label Act
                   } deriving (Eq, Show)

emptyState = Store Map.empty [] False Nothing Map.empty

-- TODO: Is this the right monad type??
-- Alternative Triple idea... Either Error, State, or BLOCK!
type M = ExceptT (Exception, Annotation) (State Store)


evalAct :: (MonadError Exception m, MonadState Store m) => Act -> m ()
evalAct = undefined

evalScene :: (MonadError Exception m, MonadState Store m) => Scene -> m ()
evalScene = undefined

evalStatement :: (MonadError Exception m, MonadState Store m) =>
  (Statement, Annotation) -> m ()
evalStatement = undefined

evalSentence :: (MonadError Exception m, MonadState Store m) =>
  Sentence -> Annotation -> m ()
evalSentence = undefined

evalExpression :: (MonadError Exception m, MonadState Store m) =>
  Expression -> Annotation -> m Value
evalExpression = undefined

execute :: Program -> Store -> (Either Exception (), Store)
execute = undefined

run :: Program -> IO ()
run program = undefined

-- TODO: Need to figure out which monad types/style we're using before
--       I can stub out the rest of these definitions
