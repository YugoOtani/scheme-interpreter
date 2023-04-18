module TokenParser where
import qualified Parser as P
import Data.List
import Parser ((.>.))

import Token
import Control.Applicative
import Data.Char as Ch
import Control.Monad

parse :: String -> Either String Toplevel
parse s = case dropWhileEnd Ch.isSpace s .>. ptop of 
    Left err -> Left err
    Right ("",res) -> Right res
    Right (s, _) -> Left $ "[" <> s <> "] remains untaken"


ptop = space0 *> 
    (TopDefine <$> pdef)
    <|> (Load <$> inBrace (P.str "load" *> space1 *> pstr))
    <|> (TopExp <$> pexp)
-- maybe need to revise 
pstr = P.char '"' *> many (P.takeIf (/= '"')) <* P.char '"' 
pdef = defvar <|> deffn 
defvar = inBrace $ do
    (_, id', exp) <- splitedBySpace3 (P.str "define") pId pexp
    return $ DefVar id'  exp

deffn = inBrace $ do
    P.str "define"
    space0 >> P.char '(' >> space0
    id' <- pId
    space0  -- ')' might appear
    prms <- pparams 
    space0 >> P.char ')' >> space0
    DefFn id' prms <$> pbody

pparams = do
    ids <- list0 space1 pId
    id' <- P.opt (space1 *> P.char '.' *> space1 *> pId)
    return $ Params ids id'

pexp = (ExpConst <$> pconst)
    <|> (ExpId <$> pId)
    <|> plambda
    <|> pquote
    <|> inBrace (FnCall <$> pexp <* space0 <*> list0 space1 pexp)
    -- (Exp *Exp)は最後?

plambda = inBrace $ do
    P.str "lambda"
    space0
    arg <- parg
    space0
    body <- pbody
    Lambda arg <$> pbody
pquote = inBrace (do
    P.str "quote"
    space0
    Quote <$> psexp) <|> Quote <$> (P.char '\'' *> psexp)
pbody = Body <$> many pdef <*> some pexp 
parg = ((\a -> Arg [a] Nothing) <$> pId)
    <|> inBrace (do
            ids <- list0 space1 pId
            rest <- P.opt (space1 *> P.char '.' *> space1 *> pId)
            return $ Arg ids rest
        )
psexp = (SConst <$> pconst) <|> (SId <$> pId)
        <|> inBrace (do
                exps <- list0 space1 psexp
                rest <- P.opt (space1 *> P.char '.' *> space1 *> psexp)
                return $ SList exps rest
            )
pconst = (Num <$> pnum) <|> (Bool <$> pbool) <|> (String <$> pstr) <|> (Nil <$ pnil) 
pnil = P.char '(' *> space0 *> P.char ')' >> return ()
pbool = (True <$ P.str "#t") <|> (False <$ P.str "#f")
pnum = read <$> some (P.takeIf Ch.isDigit)

pId = do
    s <- some $ P.takeIf Ch.isDigit <|> P.takeIf Ch.isAlpha <|> P.oneof "!$%&*+-./<=>?@^_"
    if illeagalId s 
        then P.fail $ s <> "pid fail"
        else return $ Id s

--TODO:数値となる条件
illeagalId :: P.Str -> Bool
illeagalId "." = True
illeagalId s | isNumber s = True where
    isNumber s = all Ch.isDigit s
illeagalId _ = False

space0 = many $ P.takeIf Ch.isSpace
space1 = some $ P.takeIf Ch.isSpace
splitedBySpace2 pa pb = do
    a <- pa
    space1
    b <- pb
    return (a, b)
splitedBySpace3 pa pb pc = do
    a <- pa
    space1
    b <- pb
    space1
    c <- pc
    return (a, b, c)



splitedBySpace4 pa pb pc pd = do
    a <- pa
    space1
    b <- pb
    space1
    c <- pc
    space1
    d <- pd
    return (a, b, c, d)
inBrace p = P.char '(' *> space0 *> p <* space0 <* P.char ')'

list2 :: P.Parser a -> P.Parser b -> P.Parser [b]
list2 psplit pelem = do
    elm <- pelem
    _ <- psplit
    rest <- list1 psplit pelem
    return $ elm:rest
list1 :: P.Parser a -> P.Parser b -> P.Parser [b]
list1 psplit pelem = list2 psplit pelem <|> (pure <$> pelem)

list0 :: P.Parser a -> P.Parser b -> P.Parser [b]
list0 psplit pelem = list2 psplit pelem <|> list1 psplit pelem <|> pure []