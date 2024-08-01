{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use second" #-}
module Parser where
import qualified Data.Text as T
import Control.Monad ( forM )
import Control.Applicative

newtype Data = Str Str
type Str = String
type Log = String
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
        Left la -> case b s of
            Left lb -> Left $ la <> " " <> lb
            Right rb -> Right rb
        Right ra -> Right ra

fail :: Str -> Parser a
fail s = Parser $ \t -> Left s

try :: Parser a -> Parser a
try (Parser p) = Parser $ \s -> case p s of
    Left l -> Left l
    Right (s2,res) -> Right (s, res)

takeIf :: (Char -> Bool) -> Parser Char
takeIf pred = Parser $ \s -> case s of
    "" -> Left ""
    s | pred (head s) -> Right (tail s, head s)
    s -> Left ""

char :: Char -> Parser Char
char c = takeIf (== c)

swap (a,b) = (b,a)

-- TODO: Error message
str :: Str -> Parser Str
str s = forM s $ \c -> takeIf (== c)

opt :: Parser a -> Parser (Maybe a)
opt (Parser p) = Parser $ \s -> case p s of
    Left l -> Right (s, Nothing)
    Right (s2,r) -> Right (s2, Just r)

look1 :: Parser (Maybe Char)
look1 = Parser $ \s -> case s of
    "" -> Right ("", Nothing)
    _ -> Right (s, Just (head s))


(.>.) :: Str -> Parser a -> Result a
t .>. (Parser a) = a t

oneof :: Str -> Parser Char
oneof s = Parser $ \s2 -> case s2 of
    (c:cs) | c `elem` s -> Right (cs, c)
    _ -> Left ""

noneof :: Str -> Parser Char
noneof s = Parser $ \s2 -> case s2 of
    (c:cs) | c `notElem` s2 -> Right (cs, c)
    _ -> Left ""

get :: Parser Str
get = Parser $ \s -> Right (s,s)

set :: Str -> Parser ()
set s = Parser $ \s2 -> Right (s, ()) 




    