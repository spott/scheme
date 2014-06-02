module Scheme.Parser (readExpr, readExprList) where

import Control.Monad
import Control.Monad.Error
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces)

import Scheme.Types

readExpr = readOrThrow parseExpr

readExprList = readOrThrow (endBy parseExpr spaces)

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val ->  return val

{- Parser -}
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first : rest
               return $ Atom atom

parseBool :: Parser LispVal
parseBool = do f <- char '#'
               l <- oneOf "tf"
               let b = f : [l]
               return $ case b of
                         "#t" -> Bool True
                         "#f" -> Bool False

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

digits :: String -> [Integer]
digits = map (\x -> read [x])

readBinary :: String -> Integer
readBinary s = sum $ zipWith (*) (digits . reverse $ s) (base 2)

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
parseNumber = try parseDec <|> try parseFloat <|> try parseHex <|> try parseOctal <|> try parseBinary

parseCharacter :: Parser LispVal
parseCharacter = do char '\''
                    y <- anyChar
                    char '\''
                    return $ Character y

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
parseExpr = parseNumber
        <|> parseBool
        <|> parseString
        <|> parseAtom
        <|> parseQuoted
        <|> parseCharacter
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

