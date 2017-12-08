{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction,
    FlexibleInstances #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}

module Main (main, runFile, runFileInts, runFileString) where
-- | provides command line interface for interpreting SPL code
import LanguageParser
import Evaluator
import AST (Exception)
import ExceptionPrinter

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
                            printResult $ runInt p input

runFileString :: String -> String -> IO ()
runFileString file input = do p <- parseFile file
                              printResult $ runString p input

printResult :: Either String Exception -> IO ()
printResult (Right e) = putStr $ exceptionPretty e
printResult (Left r)  = putStr r
