{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction,
    FlexibleInstances #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}

module Evaluator where

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.State (MonadState(..), StateT, State, runState, runStateT)
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import Control.Monad.Cont (MonadCont(..), ContT, Cont, runCont, runContT)


import AST

-- TODO: Store will probably change
data Store = Store { variables     :: Map CName Value
                   , activeChars   :: [CName]
                   , condition     :: Bool
                   , output        :: Maybe Char
                   , awaitingInput :: Maybe (CName, InputType)
                   , act           :: Label
                   , scene         :: Label
                   } deriving (Eq, Show)

emptyState = Store Map.empty [] False Nothing Nothing "I" "I"

data InputType = InChar | InInt deriving (Eq, Show)
data Partial a = Fail Exception | Complete | Continue (a, Store)

-- TODO: Is this the right monad type??
-- Alternative Triple idea... Either Error, State, or BLOCK!
type M a = ExceptT Exception (StateT Store (Cont (Partial a))) a


evalStatement :: (MonadCont m, MonadError Exception m, MonadState Store m) =>
  (Statement, Annotation) -> m ()
evalStatement = undefined

evalSentence :: (MonadCont m, MonadError Exception m, MonadState Store m) =>
  Sentence -> Annotation -> m ()
evalSentence = undefined

evalExpression :: (MonadCont m, MonadError Exception m, MonadState Store m) =>
  Expression -> Annotation -> m Value
evalExpression = undefined
    
-- executeBlock :: Block -> M Label
executeBlock block handleIO gotoAct gotoScene = undefined

-- executeScene :: Map Label Scene -> Block -> cont -> cont -> M Label
executeScene map b handleIO gotoAct = do
  state <- get
  case Map.lookup (scene state) map of
    Nothing -> throwError (InvalidScene (scene state))
    Just (Scene _ block) -> do
      if b == [] then executeScene map block handleIO gotoAct else do
        nextScene <- callCC $ executeBlock b handleIO gotoAct
        case nextScene of
          Nothing -> return Nothing
          Just s  -> do
            state' <- get
            put $ state' { scene = s }
            executeScene map [] handleIO gotoAct

-- executeAct :: Map Label Act -> Block -> cont -> M Block
executeAct map b handleIO = do
  state <- get
  case Map.lookup (act state) map of
    Nothing -> throwError (InvalidAct (act state))
    Just (Act _ sceneMap) -> do
      nextAct <- callCC $ executeScene sceneMap b handleIO
      case nextAct of
        Nothing -> return []
        Just a  -> do
          state' <- get
          put $ state' { act = a }
          executeAct map [] handleIO

execute :: Program -> M Block
execute (Program _ actMap) = undefined

continueFixed :: Block -> [Char] -> [Char]
continueFixed = undefined

continueIO :: Map Label Act -> Block -> IO ()
continueIO actMap block = go (Continue (block, emptyState)) where
  go (Fail e) = do
    putStrLn $ "Exception: " ++ (show e)
    return ()
  go Complete = do
    putStrLn $ "Execution complete"
    return ()
  go (Continue (block, state)) = do
    putOutput (output state)
    variables' <- readInput (awaitingInput state) (variables state)
    go $ runM (callCC $ executeAct actMap block) (updateState state variables') cont
    
  cont (Left e, _)          = Fail e
  cont (Right [], state)    = Complete
  cont (Right block, state) = Continue (block, state)

  updateState state vars = state { output = Nothing
                                 , awaitingInput = Nothing
                                 , variables = vars }
  putOutput Nothing  = return ()
  putOutput (Just c) = putChar c
  readInput Nothing m = return m
  readInput (Just (cname, InChar)) m = undefined
  readInput (Just (cname, InInt))  m = undefined

runM :: M a -> Store -> ((Either Exception a, Store) -> Partial a) -> Partial a
runM m s f = runCont (runStateT (runExceptT m) s) f

runIO :: Program -> IO ()
runIO (Program _ actMap) = undefined

