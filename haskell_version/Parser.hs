{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use second" #-}
module Parser where
import qualified Data.Text as T
import Control.Monad
import Control.Applicative

newtype Data = Str Str
type Str = String
type Result a = Either Str (Str, a)
newtype Parser a = Parser {runParse:: Str -> Result a }

pret :: a -> Parser a
pret a = Parser $ \s -> Right (s,a)

pbind :: Parser a -> (a -> Parser b) -> Parser b
pbind (Parser pa) fpab = Parser $ \s -> case pa s of
    Left l -> Left l
    Right (s2, a) -> let Parser pb = fpab a in pb s2

instance Functor Parser where
    fmap f (Parser pa) = Parser $ \s -> fmap (\(s,a) -> (s, f a)) (pa s)

instance Applicative Parser where
    pure = pret
    pf <*> pa = do
        f <- pf
        f <$> pa


instance Monad Parser where
    return = pret
    (>>=) = pbind
instance Alternative Parser where
    empty = Parser Left
    (Parser a) <|> (Parser b) = Parser $ \s -> case a s of
        Left l -> b s
        Right r -> Right r

fail :: Str -> Parser a
fail s = Parser $ \t -> Left s

takeIf :: (Char -> Bool) -> Parser Char
takeIf pred = Parser $ \s -> case s of
    "" -> Left "[readif] empty string"
    s | pred (head s) -> Right (tail s, head s)
    s -> Left $ "[readif] '" 
               <> [head s] 
               <> "' does not satisfy given condition ( ["
               <> tail s <> "] is left)"

char :: Char -> Parser Char
char c = takeIf (== c)


takeWhile :: (Char -> Bool) -> Parser Str
takeWhile pred = Parser $ \s -> Right $ swap $ span pred s 

swap (a,b) = (b,a)

-- TODO: Error message
takeStr :: Str -> Parser Str
takeStr s = forM s $ \c -> takeIf (== c)

opt :: Parser a -> Parser (Maybe a)
opt (Parser p) = Parser $ \s -> case p s of
    Left l -> Right (s, Nothing)
    Right (s2,r) -> Right (s2, Just r)


(.>.) :: Str -> Parser a -> Result a
t .>. (Parser a) = a t


