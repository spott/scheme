module Main where

import System.Environment
import Control.Monad
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces)

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

readN :: String -> N
readN s = case parse parseNumber "lisp" s of
    Left err -> I 0
    Right val -> case val of
                 Number v -> v
                 _ -> I 0

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

readExpr :: String -> LispVal
readExpr s = case parse parseExpr "lisp" s of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

{- Evaluation -}
numericBinop :: (N -> N -> N) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> N
unpackNum (Number n) = n
unpackNum (String n) = readN n

unpackNum (List [n]) = unpackNum n
unpackNum _ = I 0

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
divide (I a) (F b) = F $ (fromIntegral a) `div` b
divide (F a) (I b) = F $ a `div` (fromIntegral b)
divide (I a) (I b) = I $ a `div` b
divide (F a) (F b) = F $ a `div` b


primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop plus),
              ("-", numericBinop sub),
              ("*", numericBinop mult),
              ("/", numericBinop divide),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args


{- IO / Main function -}
main :: IO ()
main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)

