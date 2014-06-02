module Main where

import Control.Monad
import System.Environment

import Scheme.Evaluator
import Scheme.Parser
import Scheme.REPL
import Scheme.Types

main :: IO ()
main = do
    args <- getArgs
    if null args then runRepl else runOne $ args
