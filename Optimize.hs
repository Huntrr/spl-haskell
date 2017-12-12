{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction,
    FlexibleInstances #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}

module Optimize where

import AST
import Data.Map (Map, map)

type Optimizer = Program -> Program

class Optimizable a where
  optimizer :: a -> a

instance Optimizable Program where
  optimizer (Program header actMap) = Program header (Data.Map.map optimizer actMap)

instance Optimizable Expression where
  optimizer = optExpression

instance Optimizable Act where
  optimizer (Act description sceneMap) = Act description (Data.Map.map optimizer sceneMap)

instance Optimizable Scene where
  optimizer (Scene description block) = Scene description (Prelude.map optimizer block)

instance Optimizable (Statement, Annotation) where
  optimizer (Line name sentence, annotation) = (Line name (optimizer sentence), blankAnnotation)
  optimizer (statement, annotation) = (statement, annotation) -- TODO: use blankAnnotation?

instance Optimizable Sentence where
  optimizer (IfSo s) = IfSo (optimizer s)
  optimizer (IfNot s) = IfNot (optimizer s)
  optimizer (Declaration e) = Declaration (optimizer e)
  optimizer (Conditional (Comparison r e1 e2)) =
    Conditional (Comparison r (optimizer e1) (optimizer e2))
  optimizer sentence = sentence

-- simplifies expression in a program to reduce unnecessary computation
optExpression :: Expression -> Expression
optExpression (Constant e) = Constant e
optExpression (Sum e1 e2) = case (optExpression e1, optExpression e2) of
  (Constant c1, Constant c2) -> Constant (c1 + c2)
  (Constant 0, e2') -> e2'
  (e1', Constant 0) -> e1'
  (e1', e2') -> Sum e1' e2'
optExpression (Difference e1 e2) = case (optExpression e1, optExpression e2) of
  (Constant c1, Constant c2) -> Constant (c1 - c2)
  (e1', Constant 0) -> e1'
  (e1', e2') -> Difference e1' e2'
optExpression (Product e1 e2) = case (optExpression e1, optExpression e2) of
  (Constant c1, Constant c2) -> Constant (c1 * c2)
  (Constant 0, _) -> Constant 0
  (_, Constant 0) -> Constant 0
  (Constant 1, e2') -> e2'
  (e1', Constant 1) -> e1'
  (e1', e2') -> Product e1' e2'
optExpression (Quotient e1 e2) = case (optExpression e1, optExpression e2) of
  (Constant c1, Constant c2) -> Constant (c1 `div` c2)
  (e1', Constant 1) -> e1'
  (e1', e2') -> Quotient e1' e2'
optExpression (Square e) = case optExpression e of
  Constant e' -> Constant (e'^2)
  eOpt -> Square eOpt
optExpression (Cube e) = case optExpression e of
  Constant e' -> Constant (e'^3)
  eOpt -> Cube eOpt
optExpression (SquareRoot e) = case optExpression e of
  Constant e' -> Constant $ (floor . sqrt . fromIntegral) e'
  eOpt -> SquareRoot eOpt
optExpression (Twice e) = case optExpression e of
  Constant e' -> Constant (2 * e')
  eOpt -> Twice eOpt
optExpression (Mod e1 e2) = case (optExpression e1, optExpression e2) of
  (Constant c1, Constant c2) -> Constant (c1 `mod` c2)
  (e1', e2') -> Mod e1' e2'
optExpression e@(Var v) = e

