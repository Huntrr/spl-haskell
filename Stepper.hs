{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction,
    FlexibleInstances #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}

module Stepper where

import Control.Monad.State (MonadState(..), StateT, State, runState, runStateT)
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)

import Evaluator (runIO', runFixed')
import AST

runIOSteps :: Program -> Int -> IO ()
runIOSteps p n = runIO' p (Just n)

runFixedSteps :: Program -> Int -> [Int] -> Either String Exception
runFixedSteps p n = runFixed' p (Just n)
