{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction,
    FlexibleInstances #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns  #-}

module Main where
-- | provides command line interface for interpreting SPL code
import LanguageParser
import Evaluator
import AST (Exception)
import ExceptionPrinter
import Stepper

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    file:n:_ -> runFile file (Just $ read n)
    file:_ -> runFile file Nothing
    _        -> error "Proper usage: spl filename"

runFile :: String -> Maybe Int -> IO ()
runFile file n = do p <- parseFile file
                    runIO' p n

runComplete :: String -> IO ()
runComplete file = runFile file Nothing

runFileInts :: String -> [Int] -> IO ()
runFileInts file input = do p <- parseFile file
                            printResult $ runInt p input

runFileString :: String -> String -> IO ()
runFileString file input = do p <- parseFile file
                              printResult $ runString p input

evaluateFile :: String -> [Int] -> IO (Either String Exception)
evaluateFile file input = do p <- parseFile file
                             return (runInt p input)

printResult :: Either String Exception -> IO ()
printResult (Right e) = putStr $ exceptionPretty e
printResult (Left r)  = putStr r
