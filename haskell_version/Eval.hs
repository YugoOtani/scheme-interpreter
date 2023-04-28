module Eval where

import Token
import SchemeFunc
import Control.Monad
import ToStr
import qualified Data.Map as Mp
import Data.Maybe (fromJust)


evalInput :: Eval a => a -> Ctx -> Either String (String, Ctx)
evalInput top ctx = do
    (s ,ctx2@(Ctx (heap,_) _)) <- eval' top `run` ctx
    return (s, ctx2)

eval' :: Eval a => a -> CResult String
eval' top = do
    p <- eval top
    v <- valof p
    scShow v

class Eval a where
    eval :: a -> CResult Ptr

instance Eval Toplevel where
    eval (TopExp exp)= eval exp
    eval (TopDefine def) = eval def
    eval (Load fname) = error "load must be defined in main"

instance Eval Define where
    eval (DefVar (Id id) exp) = do
        val <- eval exp
        def id val
        alloc None

    eval (DefFn (Id id) arg body) = do
        env <- getEnv
        let closure = Closure (env,(arg, body))
        p <- alloc closure
        def id p
        alloc None

instance Eval Exp where
    eval (ExpId id) = eval id
    eval (ExpConst c) = eval c
    eval (FnCall fn args) = do
        fn' <- eval fn >>= valof
        args' <-  forM args eval --env
        case fn' of
            BuiltInFunc f -> f args'

            Closure (parEnv,(Params ids mid, Body defs fnbody)) ->   -- evaluate defs -> exps
                    if null fnbody then error "body has no exp" -- see TokenParser.hs.pbody
                    else do
                        p_a <- zipArgs ids mid args'
                        env <- getEnv
                        setEnv $ Frame Mp.empty parEnv
                        forM_ p_a (uncurry def)
                        forM_ defs eval
                        res <- forM fnbody eval
                        v <- getEnv
                        setEnv env
                        efail ("\n" <> tostr 0 v)
                        return $ last res

                       
            e ->  efail "is not a procedure"
    eval (Set (Id s) exp) = do
        newP <- eval exp
        maybep <- refof s
        case maybep of
            Just p -> do
                overWrite s newP
                alloc None
            Nothing -> efail $ "[set] could not find value [" <> s <>"]"
    eval (Quote sexp) = eval sexp
    eval a = efail "[eval] not defined"

instance Eval SExp where
    eval (SConst c) = eval c
    eval (SId id) = alloc (Sym id)
    eval (SList exps mexp) = do
        vals <- forM exps eval
        case mexp of
            Nothing -> listToPair vals Nothing
            Just t -> do
                tail <- eval t
                listToPair vals (Just tail)


fromId (Id s) = s

zipArgs :: [Id] -> Maybe Id -> [Ptr] -> CResult [(String, Ptr)]
zipArgs params Nothing args = if length args /= length params
                                    then efail $ "not enough argument : expected " <> show (length params) <> ", given " <> show (length args)
                                    else return $ zip (fromId <$> params) args

zipArgs params (Just (Id id)) args = if length args < length params
                                    then efail $ "not enough argument : expected " <> show (length params) <> " or more, given " <> show (length args)
                                    else let (args', rest) = splitAt (length params) args
                                            in do
                                                rest' <- listToPair rest Nothing 
                                                return $ (id, rest') : zip (fromId <$> params) args

instance Eval Id where
    eval (Id id) = do
        env <- getEnv
        maybep <- refof id
        case maybep of
            Just p -> return p
            Nothing -> efail $ "could not find value [" <> id <> "]\n" <> "note : env is \n" <> tostr 0 env


instance Eval Const where
    eval (Num n) = alloc $ PNum n
    eval (Bool b) = alloc $ PBool b
    eval (String s) = alloc $ PString s
    eval Nil = alloc PNil



ctxFn :: ([SchemeVal] -> Either String SchemeVal) -> [Ptr] -> CResult Ptr
ctxFn f ptrs =  do
    vals <- forM ptrs valof
    case f vals of
        Left l -> efail l
        Right r -> alloc r
            

globFn ::  [Char] -> SchemeVal
globFn "+"  = BuiltInFunc $ ctxFn scPlus
globFn "-" = BuiltInFunc $ ctxFn scMinus
globFn "car" = scCar
globFn "cdr" = scCdr
globFn "number?" = BuiltInFunc $ ctxFn scPNumber
globFn "*"  = BuiltInFunc $ ctxFn scMult
globFn "/"  = BuiltInFunc $ ctxFn scDiv
globFn "=" = BuiltInFunc $ ctxFn scNumEq
globFn "<" = BuiltInFunc $ ctxFn scLt
globFn "<=" = BuiltInFunc $ ctxFn scLeq
globFn ">" = BuiltInFunc $ ctxFn scGt
globFn ">=" = BuiltInFunc $ ctxFn scGeq
globFn "null?" = BuiltInFunc $ ctxFn scNull
globFn "pair?" = BuiltInFunc $ ctxFn scPPair
globFn "list?" = scPList
globFn "symbol?" = BuiltInFunc $ ctxFn scSym
globFn "cons" = scCons
globFn "list" = scList
globFn "length" = scLen
globFn "memq" = scMemq
globFn "last" = scLast
globFn "append" = scAppend
globFn "set-car!" = scSetCar
globFn "set-cdr!" = scSetCdr
globFn "boolean?" = scPBool
globFn "not" = scNot
globFn "string?" = scPStr
globFn "string-append" = scAppend
globFn "symbol->string" = scSymToStr
globFn "string->symbol" = scStrToSym
globFn "string->number" = scStrToNum
globFn "number->string" = scNumToStr
globFn "procedure?" = scPProc
globFn "eq?" = scEq
globFn "neq?" = scNeq
globFn "equal?" = scEqual
globFn _  = undefined


rootFn =  ["+","-","car", "cdr","*","/","=","<","<=",">",">=",
                                        "number?","null?","pair?","list?","symbol?","cons","list",
                                        "length","memq","last","append","set-car!","set-cdr!",
                                        "boolean?","not","string?","string-append","symbol->string",
                                        "string->symbol","string->number","number->string","procedure?",
                                        "eq?","neq?","equal?" ]