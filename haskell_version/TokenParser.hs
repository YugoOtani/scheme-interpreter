module TokenParser where
import qualified Parser as P
import Data.List
import Parser ((.>.))
import Data.Functor
import Token
import Control.Applicative
import Data.Char as Ch
import Control.Monad

parse :: String -> Either String Toplevel
parse s = case dropWhileEnd Ch.isSpace s .>. ptop of 
    Left err -> Left $ "parse error [" <> err <> "]" 
    Right ("",res) -> Right res
    Right (s, _) -> Left $ "[" <> s <> "] remains untaken"


ptop = space0 *> 
    (TopDefine <$> pdef)
    <|> (Load <$> inparen (P.str "load" *> space1 *> pstr))
    <|> (TopExp <$> pexp)
-- maybe need to revise 
pstr = P.char '"' *> many (P.takeIf (/= '"')) <* P.char '"' 
pdef = defvar <|> deffn 
defvar = inparen $ do
    P.str "define"
    space1
    id <- pId
    endOfToken
    DefVar id <$> pexp

deffn = inparen $ do
    P.str "define"
    endOfToken >> P.char '(' >> space0
    id' <- pId
    endOfToken
    prms <- pargFn 
    space0 >> P.char ')'
    DefFn id' prms <$> pbody

pargFn = do
    ids <- list0 space1 pId
    id' <- P.opt (space1 *> P.char '.' *> space1 *> pId)
    return $ Params ids id'
pexp = (ExpConst <$> pconst)
    <|> (ExpId <$> pId)
    <|> plambda
    <|> pquote
    <|> pset
    <|> plet
    <|> plet2
    <|> pletrec
    <|> pif
    <|> pcond
    <|> pand
    <|> por
    <|> pbegin
    <|> inparen (FnCall <$> pexp <* endOfToken <*> list0 endOfToken pexp)
    -- (Exp *Exp)は最後

pset = inparen $ do
    P.str "set!"
    space1
    id <- pId
    endOfToken
    Set id <$> pexp
pbind = inparen $ Binding <$> pId <* endOfToken <*> pexp
plet = inparen $ do
    P.str "let"
    endOfToken
    id <- P.opt (pId <* endOfToken)
    binds <- inparen $ list0 endOfToken pbind
    endOfToken
    Let id binds <$> pbody

    
plet2 = inparen $ do
    P.str "let*"
    endOfToken
    binds <- inparen $ list0 endOfToken pbind
    endOfToken
    Let2 binds <$> pbody


pletrec :: P.Parser Exp
pletrec = inparen $ do
    P.str "letrec"
    endOfToken
    binds <- inparen $ list0 endOfToken pbind
    endOfToken
    LetRec binds <$> pbody
pif = inparen $ do
    P.str "if"
    endOfToken
    exp1 <- pexp
    endOfToken
    exp2 <- pexp
    endOfToken
    exp3 <- P.opt pexp
    endOfToken
    return $ If exp1 exp2 exp3

pbranch = inparen $ do
    exp <- pexp
    endOfToken
    exps <- list1 endOfToken pexp
    return $ Branch exp exps
pcond = inparen $ do
    P.str "cond"
    space0
    branches <- list0 endOfToken pbranch
    space0
    exp <- P.opt $ P.str "else" *> endOfToken *> list1 endOfToken pexp
    return $ Cond branches exp
pand = inparen $ do
    P.str "and"
    endOfToken
    exps <- list0 endOfToken pexp
    return $ And exps
por = inparen $ do
    P.str "or"
    endOfToken
    exps <- list0 endOfToken pexp
    return $ Or exps
pbegin = inparen $ do
    P.str "begin"
    endOfToken
    exps <- list0 endOfToken pexp
    return $ Begin exps

plambda = inparen $ do
    P.str "lambda"
    endOfToken
    arg <- parg
    endOfToken
    body <- pbody
    return $ Lambda (arg,body)
pquote = inparen (do
    P.str "quote"
    endOfToken
    Quote <$> psexp) <|> Quote <$> (P.char '\'' *> psexp)
pbody = Body <$> list0 endOfToken pdef <*> (endOfToken *> list1 endOfToken pexp)
parg = ((\a -> Params [a] Nothing) <$> pId)
    <|> inparen (do
            ids <- list0 space1 pId
            rest <- P.opt (space1 *> P.char '.' *> space1 *> pId)
            return $ Params ids rest
        )
psexp = (SConst <$> pconst) <|> (SId <$> pId)
        <|> inparen (do
                exps <- list0 endOfToken psexp
                rest <- P.opt (space1 *> P.char '.' *> endOfToken *> psexp)
                return $ SList exps rest
            )
pconst = (Num  <$> pnum) <|> (Bool <$> pbool) <|> (String <$> pstr) <|> (Nil <$ pnil) 
pnil = P.char '(' *> space0 *> P.char ')' >> return ()
pbool = (True <$ P.str "#t") <|> (False <$ P.str "#f")
pnum = Integer . read <$> some (P.takeIf Ch.isDigit)

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

endOfToken = do
    maybec <- P.look1
    case maybec of
        Nothing -> return ()
        Just '(' -> return ()
        Just ')' -> return () -- ? 
        Just c | Ch.isSpace c -> some (P.takeIf Ch.isSpace) $> ()
        Just c -> P.fail $ "end of token expected, given '" <> show c <> "'" 
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
inparen p = P.char '(' *> space0 *> p <* space0 <* P.char ')'

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