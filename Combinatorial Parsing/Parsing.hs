-- Module declaration
module Parsing (module Parsing, module Control.Applicative) where

-- imports
import Control.Applicative
import Data.Char

-- Base Definitions
newtype Parser a = P (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (P f) = f

item :: Parser Char
item = P interior
    where 
        interior [] = Nothing
        interior (x:out) = Just (x, out)

-- Parser Classes
instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = P (\inp -> case parse p inp of
            Nothing -> Nothing
            Just (x, out) -> Just (f x, out)
        )

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure x = P (\inp -> Just (x, inp))

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> px = P (\inp -> case parse pf inp of
            Nothing -> Nothing
            Just (f, out) -> parse (fmap f px) out
        )

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
            Nothing -> Nothing
            Just (x, out) -> parse (f x) out
        )

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (const Nothing)

    -- <|> :: Parser a -> Parser a -> Parser a
    p1 <|> p2 = P (\inp -> case parse p1 inp of
            Nothing -> parse p2 inp
            something -> something
        )

-- Basic Parsers
parseIf :: (Char -> Bool) -> Parser Char
parseIf cond = do 
                x <- item
                if cond x 
                    then return x 
                    else empty

digit :: Parser Char
digit = parseIf isDigit

lower :: Parser Char
lower = parseIf isLower

upper :: Parser Char
upper = parseIf isUpper

letter :: Parser Char
letter = parseIf isAlpha

alphanum :: Parser Char
alphanum = parseIf isAlphaNum

whitespace :: Parser Char
whitespace = parseIf isSpace

char :: Char -> Parser Char
char c = parseIf (==c)

charFrom :: [Char] -> Parser Char
charFrom cs = foldl (<|>) empty (map char cs)

string :: String -> Parser String
string [] = return []
string (x:xs) = do 
                    char x
                    string xs
                    return (x:xs)

natural :: Parser Int
natural = do 
            digits <- some digit
            return $ read digits

integer :: Parser Int
integer = do 
            char '-'
            n <- natural
            return (-n)
        <|> natural

space :: Parser ()
space = do 
    many whitespace
    return ()