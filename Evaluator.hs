{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction,
    FlexibleInstances, ScopedTypeVariables #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}

module Evaluator (runIO, runInt, runString, runList) where

import Data.Char (ord, chr)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad (forM_, when, unless)
import Control.Monad.State (MonadState(..), StateT, State, runState, runStateT)
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import Control.Monad.Cont (MonadCont(..), ContT, Cont, runCont, runContT)

import ExceptionPrinter
import AST

-- Represents a partial computation, can be continued by `continue`
data Partial a = Fail Exception | Complete | Continue (a, Store) | Start Store

-- Evaluation monad
type M a = ExceptT Exception (StateT Store (Cont (Partial a))) a
runM :: M a -> Store -> ((Either Exception a, Store) -> Partial a) -> Partial a
runM m s = runCont (runStateT (runExceptT m) s)


------------------------------------------------------------------------------
-- |                           SIMPLE EVALUATORS                          | --
------------------------------------------------------------------------------
evalExpression :: forall m. (MonadError Exception m, MonadState Store m) =>
  Annotation -> CName -> Expression -> m Value
evalExpression a speaker = eval where
  eval :: Expression -> m Value
  eval (Constant x)       = return x
  eval (Sum e1 e2)        = bop (+) e1 e2
  eval (Difference e1 e2) = bop (-) e1 e2
  eval (Product e1 e2)    = bop (*) e1 e2
  eval (Mod e1 e2)        = bop mod e1 e2
  eval (Square e)         = op (^2) e
  eval (Cube e)           = op (^3) e
  eval (Quotient e1 e2)   = do
    v1 <- eval e1
    v2 <- eval e2
    when (v2 == 0) (throwError $ DivideByZero a)
    return $ v1 `div` v2
  eval (SquareRoot e)     = do
    v <- eval e
    when (v < 0) (throwError $ UnrealAnswer a)
    return $ (floor . sqrt . fromIntegral) v
  eval (Twice e)          = op (2*) e
  eval (Var ref)          = do
    state <- get
    cname <- evalRef a speaker ref
    getValue a cname

  bop f e1 e2 = do
    v1 <- eval e1
    v2 <- eval e2
    return $ f v1 v2

  op f e = do
    v <- eval e
    return $ f v

evalRef :: (MonadState Store m, MonadError Exception m) =>
  Annotation -> CName -> Reference -> m String
evalRef a speaker = f where
  f Me  = return speaker
  f You = getOther a speaker
  f (They cname) = return cname


evalComparison :: forall m. (MonadError Exception m, MonadState Store m) =>
  Annotation -> CName -> Comparison -> m Bool
evalComparison a speaker (Comparison r e1 e2) = do
  v1 <- evalExpression a speaker e1
  v2 <- evalExpression a speaker e2
  return $ (op r) v1 v2 where

  op Lt = (<)
  op Le = (<=)
  op E  = (==)
  op Ne = (/=)
  op Gt = (>)
  op Ge = (>=)

evalSentence :: forall m.
  (MonadCont m, MonadError Exception m, MonadState Store m) =>
  (() -> m (Maybe Block))
  -> (Label -> m (Maybe Label))
  -> (Label -> m (Maybe Label))
  -> Annotation -> CName -> Sentence -> m ()
evalSentence handleIO gotoAct gotoScene a cname = eval where
  eval :: Sentence -> m ()
  eval (IfSo sentence) = do
    state <- get
    case condition state of
      Nothing -> throwError $ UndefinedCondition a state
      Just b  -> when b (eval sentence)
  eval (IfNot sentence) = do
    state <- get
    case condition state of
      Nothing -> throwError $ UndefinedCondition a state
      Just b  -> unless b (eval sentence)
  eval OutputNumber     = outputInt
  eval OutputCharacter  = outputChar
  eval InputCharacter   = input InChar
  eval InputNumber      = input InInt
  eval (Declaration e)  = do
    name  <- getOther a cname
    value <- evalExpression a cname e
    state <- get
    put $ state { variables = Map.insertWith (\(v, []) (_, s) -> (v, s))
                              name (value, []) (variables state) }
  eval (Push r)         = do
    n     <- evalRef a cname r
    name <- getOther a cname
    value <- getValue a n
    state <- get
    let map = variables state
     in put $ state { variables = Map.insertWith (\(x, []) (v, xs) -> (v, x:xs)) name (value, []) map}
  eval Pop              = do
    state <- get
    name  <- getOther a cname
    new_v <- case Map.lookup name (variables state) of
      Nothing         -> return (0, [])
      Just (_, [])    -> throwError $ EmptyStack a state
      Just (_, (v:s)) -> return (v, s)
    put $ state { variables = Map.insert name new_v (variables state) }
  eval (GotoScene l)   = gotoScene l >> return ()
  eval (GotoAct l)     = gotoAct l >> return ()
  eval (Conditional c) = do
    state <- get
    cond <- evalComparison a cname c
    put $ state { condition = Just cond }

  outputChar = do
    state <- get
    cname <- getOther a cname
    value <- getValue a cname
    put $ state { output = Just $ chr value : "" }
    -- put $ state { output = Just $ show value ++ " " ++ chr value : "\n" }
    handleIO ()
    return ()

  outputInt = do
    state <- get
    cname <- getOther a cname
    value <- getValue a cname
    put $ state { output = Just $ show value }
    handleIO ()
    return ()

  input t = do
    state <- get
    n <- getOther a cname
    put $ state { awaitingInput = Just (n, t) }
    handleIO ()
    return ()





------------------------------------------------------------------------------
-- |                         EVALUATION HELPERS                           | --
------------------------------------------------------------------------------
getOther :: (MonadState Store m, MonadError Exception m) =>
  Annotation -> CName -> m String
getOther a me = do
  state <- get
  let set = (onStage state) Set.\\ (Set.singleton me)
  case Set.toList set of
    [single] -> return single
    _        -> throwError $ AmbiguousYou a set

getValue :: (MonadState Store m, MonadError Exception m) =>
  Annotation -> String -> m Value
getValue a c = do
  state <- get
  case Map.lookup c (variables state) of
    Nothing     -> return 0
    Just (v, _) -> return v




------------------------------------------------------------------------------
-- |                          BLOCK EVALUATORS                            | --
------------------------------------------------------------------------------
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
  eval (Exeunt [])           = putCharSet Set.empty
  eval (Exeunt cnames)       = forM_ cnames exitChar
  eval (Line cname sentence) = do
    state <- get
    let stage = onStage state in if Set.member cname stage
       then evalSentence handleIO gotoAct gotoScene a cname sentence
       else throwError $ NotOnStage cname a stage

  getCharSet = do
    state <- get
    return $ onStage state
  putCharSet newSet = do
    state <- get
    put $ state { onStage = newSet }
  exitChar cname  = do
    set <- getCharSet
    if Set.notMember cname set then throwError (NotOnStage cname a set) else
      putCharSet $ Set.delete cname set
  enterChar cname = do
    set <- getCharSet
    if Set.member cname set then throwError (AlreadyOnStage cname a set) else
      putCharSet $ Set.insert cname set

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




------------------------------------------------------------------------------
-- |                       FLOW CONTROL MANAGEMENT                        | --
------------------------------------------------------------------------------
-- | executeScene and executeAct are responsible for higher level flow    | --
-- | control management in the interpreter. they handle loading the       | --
-- | appropriate scenes/acts, and then passing the continuation callback  | --
-- | to executeBlock so that we can invoke GOTOs on lower level functions | --
-- |                                                                      | --
-- | they're both pretty similar in structure, acting as a loop that      | --
-- | continues to get the next scene/act every time the previous one      | --
-- | finishes (or ends early from a GOTO) until there are no more acts    | --
------------------------------------------------------------------------------
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
      next = succ s
      mNext = if Map.notMember next map then Nothing else Just next in
    case Map.lookup s map of
      Nothing -> throwError (InvalidScene s)
      Just (Scene _ block) ->
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
      next = succ a
      mNext = if Map.notMember next map then Nothing else Just next in
    case Map.lookup a map of
      Nothing -> throwError (InvalidAct a)
      Just (Act _ sceneMap) -> do
        nextAct <- callCC $ executeScene sceneMap b handleIO mNext
        case nextAct of
          Nothing -> return Nothing
          Just a  -> do
            state' <- get
            put $ state' { act = a, scene = firstScene }
            executeAct map Nothing handleIO

firstScene = 1


------------------------------------------------------------------------------
-- |                          PROGRAM CONTINUERS                          | --
------------------------------------------------------------------------------
-- | to handle IO, we model program execution as a series of "CPU bursts" | --
-- | at the end of each burst, the interpreter returns a Partial          | --
-- | representing just that one burst of execution. We need a method of   | --
-- | continuing execution from a burst (collecting input and printing     | --
-- | output as necessary after each burst) which is what the Continue     | --
-- | function is for. It will continue from a previous burst.             | --
-- |                                                                      | --
-- | we also generalize for different types of input and output, the two  | --
-- | we implement are IO (blocking) and pure [Int] input -> String output | --
-- | To do this, we generalize continue for the ProgIO class, which just  | --
-- | defines a way to get ints, characters, and print a string. Then we   | --
-- | make instances of ProgIO for IO and (State FixedIO)                  | --
------------------------------------------------------------------------------
class Monad m => ProgIO m where
  inInt  :: m Int
  inChar :: m Int
  outStr :: String -> m ()

instance ProgIO IO where
  inInt = do
    i <- getLine
    return $ read i

  inChar = do
    c <- getChar
    return $ case c of
               '\n' -> -1
               _    -> ord c

  outStr = putStr

data FixedIO = FixedIO [Int] String
instance ProgIO (State FixedIO) where
  inInt = do
    FixedIO i o <- get
    case i of
      []   -> error "Not enough inputs"
      x:xs -> do
        put (FixedIO xs o)
        return x

  inChar = inInt

  outStr s = do
    FixedIO i o <- get
    put (FixedIO i (o ++ s))


-- | This is the continue function itself, which runs a partial to completion
continue :: forall m. ProgIO m =>
  Map Label Act -> Partial (Maybe Block) -> m (Maybe Exception)
continue actMap = go where
  go :: Partial (Maybe Block) -> m (Maybe Exception)
  go (Fail e)      = return $ Just e
  go Complete      = return $ Nothing
  go (Start state) = go (Continue (Nothing, state))
  go (Continue (block, state)) = do
    putOutput (output state)
    variables' <- readInput (awaitingInput state) (variables state)
    go $ runM (callCC $ executeAct actMap block)
         (updateState state variables') cont
    
  putOutput Nothing  = return ()
  putOutput (Just c) = outStr c
  readInput Nothing m = return m
  readInput (Just (cname, t))  m = do
    val <- case t of
             InChar -> inChar
             InInt  -> inInt
    return $ Map.insertWith (\(v, _) (_, s) -> (v, s)) cname (val, []) m

  cont :: (Either Exception (Maybe Block), Store) -> Partial (Maybe Block)
  cont (Left e, _)                 = Fail e
  cont (Right Nothing, state)      = Complete
  cont (Right (Just block), state) = Continue (Just block, state)

  updateState state vars = state { output = Nothing
                                 , awaitingInput = Nothing
                                 , variables = vars }




------------------------------------------------------------------------------
-- |                            PROGRAM RUNNERS                           | --
------------------------------------------------------------------------------
-- | these run_____ functions run programs. If not for our testing code   | --
-- | these would be the only functions we'd export from this module.      | --
------------------------------------------------------------------------------
runIO :: Program -> IO ()
runIO (Program _ actMap) = continue actMap (Start emptyState) >> return ()

runFixed :: Program -> [Int] -> Either String Exception
runFixed (Program _ actMap) i = let (res, FixedIO _ o) = runState
                                      (continue actMap (Start emptyState))
                                      (FixedIO i "")
                                 in case res of
                                      Just e -> Right e
                                      Nothing -> Left o

runInt :: Program -> [Int] -> Either String Exception
runInt = runFixed

runString :: Program -> String -> Either String Exception
runString program input = let input' = stringInput input
             in runFixed program input'

runList :: Program -> [Either Int Char] -> Either String Exception
runList program input = let input' = listInput input
                         in runFixed program input'

-- | testing utils
listInput :: [Either Int Char] -> [Int]
listInput = map f where
  f (Left i)  = i
  f (Right c) = if c == '\n' then -1 else ord c

stringInput :: String -> [Int]
stringInput = map f where
  f '\n' = -1
  f c    = ord c
