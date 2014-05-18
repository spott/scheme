module Scheme.Evaluator where

import System.Environment
import Control.Monad
import Control.Monad.Error
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces)
import Scheme.Types


{- Evaluation -}
numericBinop :: (N -> N -> N) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError N
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

plus :: N -> N -> N
plus (I a) (F b) = F $ fromIntegral a + b
plus (F a) (I b) = F $ a + fromIntegral b
plus (I a) (I b) = I $ a + b
plus (F a) (F b) = F $ a + b

sub :: N -> N -> N
sub (I a) (F b) = F $ fromIntegral a - b
sub (F a) (I b) = F $ a - fromIntegral b
sub (I a) (I b) = I $ a - b
sub (F a) (F b) = F $ a - b

mult :: N -> N -> N
mult (I a) (F b) = F $ fromIntegral a * b
mult (F a) (I b) = F $ a * fromIntegral b
mult (I a) (I b) = I $ a * b
mult (F a) (F b) = F $ a * b

divide :: N -> N -> N
divide (I a) (F b) = F $ (fromIntegral a) / b
divide (F a) (I b) = F $ a / (fromIntegral b)
divide (I a) (I b) = I $ div a b
divide (F a) (F b) = F $ a / b

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop plus),
              ("-", numericBinop sub),
              ("*", numericBinop mult),
              ("/", numericBinop divide)]
              {-("mod", numericBinop mod),-}
              {-("quotient", numericBinop quot),-}
              {-("remainder", numericBinop rem)]-}

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args) 
                        (lookup func primitives)

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm
