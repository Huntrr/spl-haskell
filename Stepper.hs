{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction,
    FlexibleInstances #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}

module Stepper where

import Control.Monad.State (MonadState(..), StateT, State, runState, runStateT)
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)

import Evaluator
import AST

-- TODO: Aux functions here will come after figuring out Evaluator

stepper :: Program -> IO ()
stepper = undefined

step :: (MonadError Exception m, MonadState Store m) => Scene -> m Scene
step = undefined

executeStep :: (MonadError Exception m, MonadState Store m) =>
  Program -> Store -> m Store
executeStep = undefined

boundedStep :: (MonadError Exception m, MonadState Store m) =>
  Int -> Program -> Store -> m Scene
boundedStep = undefined
