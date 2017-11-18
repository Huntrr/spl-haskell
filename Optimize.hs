module Optimize where

import AST

type Optimizer = Program -> Program

-- sequences all of our optimizers to produce the best cost
optimizer :: Optimizer
optimizer = undefined

-- simplifies expression in a program to reduce unnecessary computation
expressionOptimizer :: Optimizer
expressionOptimizer = undefined

optExpression :: Expression -> Expression
optExpression = undefined

-- propagates constants to hopefully get rid of pure constant variables
-- there are lots of these in SPL programs so this is very useful!
constantProp :: Optimizer
constantProp = undefined


-- TODO: other optimizers?
