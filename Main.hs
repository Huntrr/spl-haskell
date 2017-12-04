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

runFileInts :: String -> [Int] -> IO ()
runFileInts file input = do p <- parseFile file
                            putStr $ runInt p input

runFileString :: String -> String -> IO ()
runFileString file input = do p <- parseFile file
                              putStr $ runString p input
