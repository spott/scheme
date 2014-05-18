module Scheme.Parser where

import System.Environment
import Control.Monad
import Control.Monad.Error
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces)

import Scheme.Types

readExpr :: String -> ThrowsError LispVal
readExpr s = case parse parseExpr "lisp" s of
    Left err -> throwError $ Parser err
    Right val -> return val

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

