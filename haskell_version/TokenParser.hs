import qualified Parser as P

import Parser ((.>.))
import Token
import Control.Applicative
import Data.Char as Ch

main = do
    s <- getLine
    print s
    print $ s .>. ptop


ptop = (TopExp <$> pexp) 
    <|> (TopDefine <$> pdef) 
    <|> (Load <$> inBrace (P.takeStr "load" *> takeSpace1 *> pstr))
-- maybe need to revise 
pstr = P.char '"' *> P.takeWhile (/= '"') <* P.char '"'
pdef =  P.fail ""
pparams = P.fail ""
pexp = P.fail ""
pcond = P.fail ""
pbody = P.fail ""
parg = P.fail ""
pbinding = P.fail ""
psexp = P.fail ""
pconst = P.fail ""
pId = P.fail ""


takeSpace0 = P.opt takeSpace1
takeSpace1 = P.takeWhile Ch.isSpace
splitedBySpace2 pa pb = do
    a <- pa
    takeSpace1
    b <- pb
    return (a, b)
splitedBySpace3 pa pb pc = do
    a <- pa
    takeSpace1
    b <- pb
    takeSpace1
    c <- pc
    return (a, b, c) 
inBrace p = takeSpace0 *> P.char '(' *> takeSpace0 *> p <* takeSpace0 <* P.char ')'
