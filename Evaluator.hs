{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction,
    FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}

module Evaluator where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad (forM_)
import Control.Monad.State (MonadState(..), StateT, State, runState, runStateT)
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import Control.Monad.Cont (MonadCont(..), ContT, Cont, runCont, runContT)


import AST

-- TODO: Store will probably change
data Store = Store { variables     :: Map CName Value
                   , onStage   :: Set CName
                   , condition     :: Bool
                   , output        :: Maybe Char
                   , awaitingInput :: Maybe (CName, InputType)
                   , act           :: Label
                   , scene         :: Label
                   } deriving (Eq, Show)

emptyState = Store Map.empty Set.empty False Nothing Nothing "I" "I"

data InputType = InChar | InInt deriving (Eq, Show)
data Partial a = Fail Exception | Complete | Continue (a, Store) | Start Store

-- TODO: Is this the right monad type??
-- Alternative Triple idea... Either Error, State, or BLOCK!
type M a = ExceptT Exception (StateT Store (Cont (Partial a))) a

evalExpression :: (MonadError Exception m, MonadState Store m) =>
  Expression -> Annotation -> m Value
evalExpression = undefined

evalSentence :: forall m.
  (MonadCont m, MonadError Exception m, MonadState Store m) =>
  (() -> m (Maybe Block))
  -> (Label -> m (Maybe Label))
  -> (Label -> m (Maybe Label))
  Annotation -> Character -> Sentence -> m ()
evalSentence handleIO gotoAct gotoScene a = eval where
  eval :: CName -> Sentence -> m ()
  eval char sentence = undefined

evalStatement :: forall m.
  (MonadCont m, MonadError Exception m, MonadState Store m) =>
  (() -> m (Maybe Block))
  -> (Label -> m (Maybe Label))
  -> (Label -> m (Maybe Label))
  -> (Statement, Annotation)
  -> m ()
evalStatement handleIO gotoAct gotoScene (s, a) = eval s where
  eval :: Statement -> m ()
  eval (Enter cnames)        = forM_ cnames enterChar
  eval (Exit cname)          = exitChar cname
  eval (Exeunt cnames)       = forM_ cnames exitChar
  eval (Line cname sentence) = evalSentence handleIO gotoAct gotoScene a
                                 cname sentence

  getCharSet = do
    state <- get
    return $ onStage state
  putCharSet newSet = do
    state <- get
    put $ state { onStage = newSet }
  exitChar cname  = do
    set <- getCharSet
    if Set.notMember cname set then throwError (NotOnStage cname a) else
      putCharSet $ Set.delete cname set
  enterChar cname = do
    set <- getCharSet
    if Set.notMember cname set then throwError (NotOnStage cname a) else
      putCharSet $ Set.delete cname set

executeBlock :: forall m.
  (MonadState Store m, MonadCont m, MonadError Exception m) =>
   Block
   -> (Maybe Block -> m (Maybe Block))
   -> (Maybe Label -> m (Maybe Label))
   -> (Maybe Label)
   -> (Maybe Label -> m (Maybe Label))
   -> m (Maybe Label)
executeBlock b handleIO gotoAct nextScene gotoScene = go b where
  go :: Block -> m (Maybe Label)
  go [] = return nextScene
  go (s:rest) = do
    evalStatement (\() -> handleIO (Just rest))
      (gotoAct . Just) (gotoScene . Just) s
    go rest

executeScene :: (MonadState Store m, MonadCont m, MonadError Exception m) =>
  Map Label Scene
  -> Maybe Block
  -> (Maybe Block -> m (Maybe Block))
  -> Maybe Label
  -> (Maybe Label -> m (Maybe Label))
  -> m (Maybe Label)
executeScene map b handleIO nextAct gotoAct = do
  state <- get
  let s = scene state
      next = nextLabel s
      mNext = if Map.notMember next map then Nothing else Just next in
    case Map.lookup s map of
      Nothing -> throwError (InvalidScene s)
      Just (Scene _ block) -> do
        case b of
          Nothing -> executeScene map (Just block) handleIO nextAct gotoAct
          Just block' -> do
            nextScene <- callCC $ executeBlock block' handleIO gotoAct mNext
            case nextScene of
              Nothing -> return nextAct
              Just s  -> do
                state' <- get
                put $ state' { scene = s }
                executeScene map Nothing handleIO nextAct gotoAct

executeAct :: (MonadState Store m, MonadCont m, MonadError Exception m) =>
  Map Label Act
  -> Maybe Block
  -> (Maybe Block -> m (Maybe Block))
  -> m (Maybe Block)
executeAct map b handleIO = do
  state <- get
  let a = act state
      next = nextLabel (a)
      mNext = if Map.notMember next map then Nothing else Just next in
    case Map.lookup a map of
      Nothing -> throwError (InvalidAct a)
      Just (Act _ sceneMap) -> do
        nextAct <- callCC $ executeScene sceneMap b handleIO mNext
        case nextAct of
          Nothing -> return Nothing
          Just a  -> do
            state' <- get
            put $ state' { act = a }
            executeAct map Nothing handleIO

nextLabel :: Label -> Label
nextLabel "I" = "II"
nextLabel "II" = "III"
nextLabel "III" = "IV"
nextLabel "IV" = "V"
nextLabel "V" = "VI"
nextLabel "VI" = "VII"
nextLabel "VII" = "VIII"
nextLabel "VIII" = "IX"
nextLabel "IX" = "X"
nextLabel _ = undefined

continueFixed :: Map Label Act -> Block -> [Char] -> [Char]
continueFixed = undefined

continueIO :: Map Label Act -> Partial (Maybe Block) -> IO ()
continueIO actMap partial = go partial where
  go :: Partial (Maybe Block) -> IO ()
  go (Fail e) = do
    putStrLn $ "Exception: " ++ (show e)
    return ()
  go Complete = do
    putStrLn $ "Execution complete"
    return ()
  go (Start state) = go $ Continue (Nothing, state)
  go (Continue (block, state)) = do
    putOutput (output state)
    variables' <- readInput (awaitingInput state) (variables state)
    go $ runM (callCC $ executeAct actMap block)
         (updateState state variables') cont
    
  cont :: (Either Exception (Maybe Block), Store) -> Partial (Maybe Block)
  cont (Left e, _)                 = Fail e
  cont (Right Nothing, state)      = Complete
  cont (Right (Just block), state) = Continue (Just block, state)

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
runIO (Program _ actMap) = continueIO actMap (Start emptyState)
