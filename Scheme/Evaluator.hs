module Scheme.Evaluator where

import Control.Monad
import Control.Monad.Error
import Data.IORef
import Debug.Trace
import Numeric
import System.Environment
import System.IO

import Scheme.Parser
import Scheme.Types


{- Numerical Evaluation -}
numericBinop :: (N -> N -> N) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

integerBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
integerBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
integerBinop op params = mapM unpackInt params >>= return . Number . I . foldl1 op

unpackNum :: LispVal -> ThrowsError N
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackInt :: LispVal -> ThrowsError Integer
unpackInt (Number num) = case num of
                           I n -> return n
                           F _ -> throwError $ TypeMismatch "integer" $ Number num
unpackInt (List [n]) = unpackInt n
unpackInt notNum = throwError $ TypeMismatch "integer" notNum

raiseOp :: (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> N -> N -> N
raiseOp opI opF (I a) (F b) = F $ fromIntegral a `opF` b
raiseOp opI opF (F a) (I b) = F $ a `opF` fromIntegral b
raiseOp opI opF (F a) (F b) = F $ opF a b
raiseOp opI opF (I a) (I b) = I $ opI a b

{- Bool Evaluation -}

boolBinop :: (LispVal -> ThrowsError a) ->  (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                                then throwError $ NumArgs 2 args
                                else do left <- unpacker $ head args
                                        right <- unpacker $ args !! 1
                                        return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Character s) = return $ show s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackChar :: LispVal -> ThrowsError Char
unpackChar (Character s) = return s
unpackChar notCharacter = throwError $ TypeMismatch "character" notCharacter

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

{- List primitives -}

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [DottedList [xs] x] = return x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Character arg1), (Character arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
                                                    (and $ map eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left err -> False
                               Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                      [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop $ raiseOp (+) (+)),
              ("-", numericBinop $ raiseOp (-) (-)),
              ("*", numericBinop $ raiseOp (*) (*)),
              ("/", numericBinop $ raiseOp div (/)),
              ("mod", integerBinop mod),
              ("quotient", integerBinop quot),
              ("remainder", integerBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                                ++ map (makeFunc PrimitiveFunc) primitives)
    where makeFunc constructor (var, func) = (var, constructor func)

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine stdin) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename


apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = trace ("    primitivefunc " ++ show args) $  liftThrows $ func args
--apply (IOFunc func) args = trace ("    iofunc " ++ show args) $ func args
apply (Func params varargs body closure) args = trace ("    func " ++ show args) $
        if num params /= num args && varargs == Nothing
            then throwError $ NumArgs (num params) args
            else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
        where remainingArgs = drop (length params) args
              num = toInteger . length
              evalBody env = liftM last $ mapM (eval env) body
              bindVarArgs arg env = case arg of
                                Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                                Nothing -> return env

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do result <- eval env pred
                                                    case result of
                                                         Bool False -> eval env alt
                                                         otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
        eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
        trace ("[define:" ++ show var ++ ": " ++ show form ++ "]") $
        eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
        makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
        makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
        trace ("[lambda:" ++ show params ++ ": " ++ show body ++ "]") $
        makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
        trace ("[lambda:" ++ show params ++ ": " ++ show varargs ++ ": " ++ show body ++ "]") $
        makeVarargs varargs env [] body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
        makeVarargs varargs env [] body
eval env (List [Atom "load", String filename]) =
    load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = trace (show function ++ ": ") $ do
        func <- eval env function
        argVals <- mapM (eval env) args
        trace ( "  " ++ show argVals ) $ apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

{- Variables and functions -}

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
                       env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
        alreadyDefined <- liftIO $ isBound envRef var
        if alreadyDefined
            then setVar envRef var value >> return value
            else liftIO $ do
                valueRef <- newIORef value
                env <- readIORef envRef
                writeIORef envRef ((var, valueRef) : env)
                return value

bindVars :: Env -> [(String,LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)

--makeFunc :: (Maybe String) -> Env -> [String] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal
