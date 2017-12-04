{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction,
    FlexibleInstances #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}

module Main where
-- | provides command line interface for interpreting SPL code
import LanguageParser
import Evaluator

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    file:_ -> runFile file
    _        -> error "Proper usage: spl filename"

runFile :: String -> IO ()
runFile file = do p <- parseFile file
                  runIO p

runFileWithInput :: String -> [String] -> IO ()
runFileWithInput file input = do p <- parseFile file
                                 putStr $ runFixed p input
