{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction,
    FlexibleInstances #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}

module PrettyPrinter where
  
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP

import AST

class PP a where
  pp :: a -> Doc


render :: PP a => a -> String
render = PP.render . pp

instance PP Program where
  pp _ = undefined
