import qualified Parser as P

import Parser ((.>.))
import Token
import Control.Applicative
import Data.Char as Ch
import Control.Monad

main = do
    s <- getLine
    print s
    case s .>. test of
        Right (r,s) -> print (r,s)
        Left l -> putStrLn ("fail" <> l)


test = ptop

ptop :: P.Parser Toplevel
ptop = space0 *> (TopExp <$> pexp)
    <|> (TopDefine <$> pdef)
    <|> (Load <$> inBrace (P.str "load" *> space1 *> pstr))
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


pexp :: P.Parser Exp
pexp = (ExpConst <$> pconst)
    <|> (ExpId <$> pId)
    <|> inBrace (FnCall <$> pexp <* space1 <*> list0 space1 pexp)
    -- (Exp *Exp)は最後?
pcond = P.fail ""
pbody = Body <$> many pdef <*> some pexp 
parg = P.fail ""
pbinding = P.fail ""
psexp = P.fail ""
pconst = (Num <$> pnum) <|> (Bool <$> pbool) <|> (String <$> pstr) <|> (Nil <$ pnil) 
pnil = P.char '(' *> space0 *> P.char ')' >> return ()
pbool = (True <$ P.str "#t") <|> (False <$ P.str "#f")
--TODO
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