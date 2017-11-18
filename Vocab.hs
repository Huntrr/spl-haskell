{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction,
    FlexibleInstances #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}

module Vocab where

import Test.QuickCheck

import AST

character :: Gen Character
character = elements ["Romeo", "Juliet"]

-- TODO: etc (probably reading from files!)
