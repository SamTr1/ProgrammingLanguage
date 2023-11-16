{-# LANGUAGE LambdaCase #-}

-- Functional parsing library from chapter 13 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.

module Parsing (module Parsing, module Control.Applicative) where

import Control.Applicative
import Data.Char ( isAlpha, isAlphaNum, isDigit, isLower, isSpace, isUpper )
import Control.Monad ( ap, liftM )

--------------------------------------------------------------------------------
-- Basic Definitions
--------------------------------------------------------------------------------

newtype Parser a = P { runParser :: String -> [(a,String)] }

item :: Parser Char
item = P (\case []     -> []
                (x:xs) -> [(x,xs)])

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
   pure = return
   (<*>) = ap

instance Monad Parser where
  return v = P (\inp -> [(v,inp)])
  p >>= f = P (\inp -> case runParser p inp of
                         []        -> []
                         [(v,out)] -> runParser (f v) out)

instance Alternative Parser where
  empty = P (const [])
  p <|> q = P (\inp -> case runParser p inp of
                         []        -> runParser q inp
                         [(v,out)] -> [(v,out)])

--------------------------------------------------------------------------------
-- Derived Primitives
--------------------------------------------------------------------------------

failure :: Parser a
failure =  empty

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit, lower, upper, letter, alphanum, sym :: Parser Char
digit    = sat isDigit
lower    = sat isLower
upper    = sat isUpper
letter   = sat isAlpha
alphanum = sat isAlphaNum
sym      = sat (`elem` "!$%&|*/+-:<=?>@^_~")

char :: Char -> Parser Char
char x = sat (== x)

oneOf :: [Char] -> Parser Char
oneOf = foldr ((<|>) . char) failure

string :: String -> Parser String
string ""     = return ""
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x  <- lower <|> sym
           xs <- many (alphanum <|> sym)
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat

--------------------------------------------------------------------------------
-- Handling Spacing
--------------------------------------------------------------------------------

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)
