module Main where

import System.Environment
import Control.Monad

import Scheme.Parser
import Scheme.Types
import Scheme.Evaluator

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled
