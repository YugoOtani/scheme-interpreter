module Eval where

import Token
import SchemeFunc
import Control.Monad
import ToStr
import qualified Data.Map as Mp

type Heap = (Mp.Map Ptr SchemeVal, Ptr)

valof :: String -> CResult (Maybe SchemeVal)
valof s = CResult $ \ctx@(Ctx (heap, _) env) -> do
        ptr <- getVar s env
        case (Mp.!?) heap ptr of
            Just v -> Right (Just v, ctx)
            Nothing -> error "segmentation fault"
            
alloc :: String -> SchemeVal -> CResult ()
alloc s v = CResult $ \(Ctx (heap, ptr) env) -> Right ((),Ctx (Mp.insert (ptr+1) v heap, ptr+1) (insertVar s (ptr+1) env))

assign :: String -> SchemeVal -> CResult ()
assign s v = CResult $ \(Ctx (heap, top) env) -> do
                vptr <- getVar s env
                case heap Mp.!? vptr of
                    Nothing -> error "segmentaion fault"
                    Just _ -> do
                        let heap' = Mp.insert vptr v heap
                        Right ((), Ctx (heap',top) env)


zeroCtx = Ctx (Mp.empty,0) (Frame Mp.empty NilFrame)
initialCtx :: Ctx
initialCtx = case forM_ rootFn (\s -> alloc s (globFn s)) `run` zeroCtx of
            Left l -> error $ "[init ctx error]" <> l
            Right (_,ctx) -> ctx
data Ctx = Ctx {heap:: !Heap, env:: !Env} 
instance Show Ctx where
    show (Ctx (heap,_) env) = "heap\n" <> show heap <> "\nenv\n" <> show env
newtype CResult a = CResult {run :: Ctx -> Either String (a, Ctx)}
instance Functor CResult where
    fmap f (CResult s) = CResult $ \e -> do
        (a, ctx) <- s e
        return (f a, ctx)

instance Applicative CResult where
    pure a = CResult $ \e -> Right (a, e)
    CResult ab <*> CResult a = CResult $ \ctx1 -> do
        (ab', ctx2) <- ab ctx1
        (a' , ctx3) <- a ctx2
        return (ab' a', ctx3)

instance Monad CResult where
    return a = CResult $ \e -> Right (a,e)
    CResult a >>= ab = CResult $ \c1 -> do
        (a',c2) <- a c1
        let (CResult b) = ab a'
        (b', c3) <- b c2
        return (b',c3)

efail :: String -> CResult a
efail s = CResult $ \_ -> Left s 

getEnv :: CResult Env
getEnv = CResult $ \c -> Right (env c,c)

setEnv :: Env -> CResult ()
setEnv e = CResult $ \c -> Right ((), Ctx (heap c) e)


evalInput top = run (eval top)

class Eval a where
    eval :: a -> CResult SchemeVal

instance Eval Toplevel where
    eval (TopExp exp)= eval exp
    eval (TopDefine def) = eval def
    eval (Load fname) = error "load must be defined in main"

instance Eval Define where
    eval (DefVar (Id id) exp) = do 
        val <- eval exp
        alloc id val
        return None

    eval (DefFn (Id id) arg body) = do
        env <- getEnv
        let closure = Closure (env,(arg, body))
        alloc id closure
        return None

instance Eval Exp where
    eval (ExpId id) = eval id
    eval (ExpConst c) = eval c
    eval (FnCall fn args) = do
        fn' <- eval fn 
        args' <-  forM args eval --env
        case fn' of 
            BuiltInFunc f -> do
                    case f args' of
                        Left l -> efail l
                        Right r -> return r
            Closure (parEnv,(Params ids mid, Body defs fnbody)) ->   -- evaluate defs -> exps
                    if null fnbody then error "body has no exp" -- see TokenParser.hs.pbody
                    else do
                        case zipArgs ids mid args' of
                            Left l -> efail l
                            Right params_args -> do
                                env <- getEnv
                                setEnv $ Frame Mp.empty parEnv
                                forM_ params_args (uncurry alloc)
                                forM_ defs eval
                                res <- forM fnbody eval
                                setEnv env
                                return $ last res
                         
                        -- (define (f y) (define x y) (define z x) (+ x y z)) これがエラーにならない
                    
            e ->  efail $ show e <> "is not a procedure"
    eval (Set (Id s) exp) = do
        sval <- eval exp
        maybev <- valof s 
        case maybev of
            Just v -> do
                assign s sval
                return None
            Nothing -> efail $ "could not find value [" <> s <>"]"
    eval (Quote sexp) = eval sexp
    eval a = efail $ show a <> " is not defined" 

instance Eval SExp where
    eval (SConst c) = eval c
    eval (SId id) = return $ Sym id
    eval (SList exps mexp) = do
        vals <- forM exps eval
        case mexp of
            Nothing -> return $ listToPair vals Nothing
            Just t -> do
                tail <- eval t
                return $ listToPair vals (Just tail)

        
fromId (Id s) = s

zipArgs :: [Id] -> Maybe Id -> [SchemeVal] -> Either [Char] [(String, SchemeVal)]
zipArgs params Nothing args = if length args /= length params 
                                    then Left $ "not enough argument : expected " <> show (length params) <> ", given " <> show (length args)
                                    else Right $ zip (fromId <$> params) args

zipArgs params (Just (Id id)) args = if length args < length params 
                                    then Left $ "not enough argument : expected " <> show (length params) <> " or more, given " <> show (length args)
                                    else let (args', rest) = splitAt (length params) args 
                                            in Right $ (id, listToPair rest Nothing) : zip (fromId <$> params) args

instance Eval Id where
    eval (Id id) = do
        env <- getEnv
        maybev <- valof id
        case maybev of
            Just v -> return v
            Nothing -> efail $ "could not find value [" <> id <> "]\n" <> "note : env is \n" <> tostr 0 env 


instance Eval Const where
    eval (Num n) = return $ PNum n
    eval (Bool b) = return $ PBool b
    eval (String s) = return $ PString s
    eval Nil = return PNil


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
    
    
rootFn =  ["+","-","car", "cdr","*","/","=","<","<=",">",">=",
                                        "number?","null?","pair?","list?","symbol?","cons","list",
                                        "length","memq","last","append","set-car!","set-cdr!",
                                        "boolean?","not","string?","string-append","symbol->string",
                                        "string->symbol","string->number","number->string","procedure?",
                                        "eq?","neq?","equal?" ] 