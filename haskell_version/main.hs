{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
import Parser
import TokenParser
import Token
import ToStr ( ToStr(tostr) )
import Eval
import SchemeFunc
import qualified Data.Map as Mp
import Control.Monad (forM, forM_)
import Data.List

main = do
    loop 1 rootFrame where
        loop i env = do
            putStr $ "Haskeme[" <> show i <> "] > "
            input <- getLine
            newEnv <- exec env input
            loop (i+1) newEnv

exec env input = case parse input of
    Right (Load fname) -> do
        s <- readFile fname
        case parseMany s of
            Left err -> do
                putStrLn err
                return env
            Right tops -> do
                case run (forM_ tops eval) env of
                    Left err -> do
                        putStrLn err
                        return env
                    Right (_,newEnv) -> do
                        putStrLn "[load] success"
                        return newEnv

    Right parseRes -> case evalInput env parseRes of
                        Left l -> do
                            putStrLn $ tostr 0 parseRes
                            putStrLn l
                            return env
                        Right (r,newEnv) -> do
                            -- putStrLn $ tostr 0 parseRes
                            print r 
                            -- putStrLn $ "env:\n" <> tostr 0 newEnv
                            return newEnv
    Left l -> do
        putStrLn $ "parse error" <> l
        return env


globFn ::  [Char] -> SchemeVal
globFn "+"  = scPlus
globFn "-" = scMinus
globFn "car" = scCar
globFn "cdr" = scCdr
globFn "number?" = scPNumber
globFn "*"  = scMult
globFn "/"  = scDiv
globFn "=" = scNumEq
globFn "<" = scLt
globFn "<=" = scLeq
globFn ">" = scGt
globFn ">=" = scGeq
globFn "null?" = scNull
globFn "pair?" = scPPair
globFn "list?" = scPList
globFn "symbol?" = scSym 
globFn "cons" = scCons 
globFn "list" = scList
globFn "length" = scLen 
globFn "memq" = scMemq
globFn "last" = scLast
globFn "append" = scAppend
globFn "set-car!" = scSetCar
globFn "set-cdr!" = scSetCdr
globFn "boolean?" = None
globFn "not" = None
globFn "string?" = None
globFn "string-append" = None
globFn "symbol->string" = None
globFn "string->symbol" = None
globFn "string->number" = None
globFn "number->string" = None
globFn "procedure?" = None
globFn "eq?" = None
globFn "neq?" = None
globFn "equal?" = None
globFn _  = undefined

rootFrame :: Env
rootFrame = let root = defRootFrame root
                defRootFrame env = Frame 
                                (Mp.fromList ((\s -> (s, globFn s)) 
                                    <$> ["+","-","car", "cdr","*","/","=","<","<=",">",">=",
                                        "number?","null?","pair?","list?","symbol?","cons","list",
                                        "length","memq","last","append","set-car!","set-cdr!",
                                        "boolean?","not","string?","string-append","symbol->string",
                                        "string->symbol","string->number","number->string","procedure?",
                                        "eq?","neq?","equal?" ])) 
                                NilFrame
            in root

