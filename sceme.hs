module Main where

import System.Environment
import Control.Monad
import Control.Monad.Error
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces)

{- LispError data -}
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected 
                                  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError

{- Error handling -}
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

{- LispVal Data Type -}
data N = I Integer | F Double

showN :: N -> String
showN (I integer) = show integer
showN (F float) = show float

instance Show N where show = showN

data LispVal = Atom String 
             | List [LispVal] 
             | DottedList [LispVal] LispVal 
             | Number N
             | String String 
             | Bool Bool

{-readN :: String -> ThrowsError N-}
{-readN s = case parse parseNumber "lisp" s of-}
    {-Left err -> throwError $ Parser err-}
    {-Right val -> case val of-}
                 {-Number v -> return v-}
                 {-_ -> return $ I 0-}

--show :: Show LispVal
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"

showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

{- Parser -}
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of 
                          "#t" -> Bool True
                          "#f" -> Bool False
                          otherwise -> Atom atom

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail


{- Parsing Numbers -}

base :: Integer -> [Integer]
base n = map (\x -> n^x) [0,1..]

digits :: [Char] -> [Integer]
digits = map (\x -> read [x])



readBinary :: String -> Integer
readBinary s = foldl1 (+) $ map ( \(x,y) -> x * y ) (zip (digits . reverse $ s) (base 2))

parseOctal :: Parser LispVal
parseOctal = do string "#o"
                y <- many1 octDigit
                [(z,_)] <- return . readOct $ y
                return . Number . I $ z

parseBinary :: Parser LispVal
parseBinary = do string "#b"
                 y <- many1 ( oneOf "01" )
                 z <- return . readBinary $ y
                 return . Number . I $ z

parseHex :: Parser LispVal
parseHex = do string "#x"
              y <- many1 hexDigit
              [(z,_)] <- return . readHex $ y
              return . Number . I $ z

parseDec :: Parser LispVal
parseDec = do optional $ string "#d"
              y <- many1 digit
              lookAhead ( oneOf " )")
              z <- return . read $ y
              return . Number . I $ z

parseFloat :: Parser LispVal
parseFloat = do y <- many1 (digit <|> oneOf ".eE")
                [(z,_)] <- return . readFloat $ y
                return . Number . F $ z

parseNumber :: Parser LispVal
parseNumber = (try parseDec) <|> (try parseFloat) <|> (try parseHex) <|> (try parseOctal) <|> (try parseBinary)

parseString :: Parser LispVal
parseString = do char '"'
                 x <- manyTill anyChar (try ( string "\"" ))
                 return $ String x

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- (try parseList) <|> parseDottedList
               char ')'
               return x

readExpr :: String -> ThrowsError LispVal
readExpr s = case parse parseExpr "lisp" s of
    Left err -> throwError $ Parser err
    Right val -> return val

{- Evaluation -}
numericBinop :: (N -> N -> N) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError N
unpackNum (Number n) = return n
--unpackNum (String n) = readN n

unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum
unpackNum _ = return $ I 0

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
              --("mod", numericBinop mod),
              --("quotient", numericBinop quot),
              --("remainder", numericBinop rem)]

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


{- IO / Main function -}
main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled
{-main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)-}

