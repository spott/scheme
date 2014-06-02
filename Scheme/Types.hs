{-# LANGUAGE ExistentialQuantification #-}
module Scheme.Types where


import Control.Monad.Error
import Data.IORef
--import           Numeric
--import           System.Environment
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)

data N = I Integer | F Double

showN :: N -> String
showN (I integer) = show integer
showN (F float) = show float

eqN :: N -> N -> Bool
eqN (I a) (I b) = (==) a b
eqN (F a) (F b) = (==) a b
eqN (F a) (I b) = (==) a $ fromIntegral b
eqN (I a) (F b) = (==) b $ fromIntegral a

ltN :: N -> N -> Bool
ltN (I a) (I b) = (<=) a b
ltN (F a) (F b) = (<=) a b
ltN (F a) (I b) = (<=) a $ fromIntegral b
ltN (I a) (F b) = (<=) b $ fromIntegral a

instance Show N where show = showN
instance Eq N where (==) = eqN
instance Ord N where (<=) = ltN

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number N
             | String String
             | Character Char
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String], vararg :: Maybe String, body :: [LispVal], closure :: Env}
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

--display :: annotated lispval:
displayVal :: LispVal -> String
displayVal (String contents) = "string \"" ++ contents ++ "\"" ++ ";"
displayVal (Atom name) = "atom " ++ name ++ ";"
displayVal (Number contents) = "number " ++ show contents ++ ";"
displayVal (Bool True) = "Bool #t;"
displayVal (Bool False) = "Bool #f;"
displayVal (Character c) = "character " ++show c ++ ";"
displayVal (List contents) = "List (" ++ unwordsList displayVal contents ++ ");"
displayVal (DottedList h t) = "DottedList (" ++ unwordsList displayVal h ++ " . " ++ displayVal t ++ ");"
displayVal (PrimitiveFunc _) = "<primitive>;"
displayVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
        "(lambda (" ++ unwords (map show args) ++
            (case varargs of
                Nothing -> ""
                Just arg -> " . " ++ arg) ++ ") " ++ (concatMap displayVal body) ++ ");"
displayVal (Port _) = "<IO port>;"
displayVal (IOFunc _) = "<IO primitive>;"



--show :: Show LispVal
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character c) = show c
showVal (List contents) = "(" ++ unwordsList showVal contents ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList showVal h ++ " . " ++ showVal t ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
        "(lambda (" ++ unwords (map show args) ++
            (case varargs of
                Nothing -> ""
                Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

unwordsList :: (LispVal -> String) -> [LispVal] -> String
unwordsList d = unwords . map d --(\x -> (++) (showVal x) "\n")

instance Show LispVal where show = displayVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (NumArgs expected found) = "Expected " ++ show expected
                                  ++ " args; found values " ++ unwordsList displayVal found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (Default s) = show s

instance Show LispError where show = showError

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val


{- The Environment type.  This holds the name of the variable, and
- a reference to the LispValue. -}
type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

{-showEnv :: Env -> IO String-}
{-showEnv env = liftM . concat $ fmap (\(x,y) -> x ++ (show $ ioReftoval y)) (ioReftoval env)-}
    {-where ioReftoval = readIORef-}

{- The error monad type.  We need to be able to move around an object that
- can talk about accesses to our Env.  -}
type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

{- drops out of the ThrowsError monad monad down into just the IO monad -}
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue
