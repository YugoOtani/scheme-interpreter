{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
import Parser
import TokenParser
import Token
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


toSchemeVal :: Env -> [Char] -> SchemeVal
toSchemeVal root "+"  = BuiltInFunc $ \args -> do
                        a <- forM args (\e -> case e of
                                Const (Num i) -> Right i
                                _ -> Left "[+] can only be applied between numbers")
                        Right $ Const $ Num $ sum' a 
toSchemeVal root "-" = BuiltInFunc f where
                        f [] = Left "[-] must have 1 argument"
                        f (Const (Num NaN) : xs) = Right $ Const $ Num NaN
                        f (Const (Num (Integer i)) : xs) = do
                            a <- forM xs (\e -> case e of
                                Const (Num i) -> Right i
                                _ -> Left "[-] can only be applied between numbers")
                            let ans = case sum' a of
                                        NaN -> NaN
                                        (Integer i2) -> Integer (i - i2)
                            Right $ Const $ Num ans
                        f _ = Left "'-' can only be applied between numbers"
toSchemeVal root "car" = BuiltInFunc f where
                        f [Pair (h,t)] = Right h
                        f [x] = Left "[car] can only be applied to List"
                        f s = Left $ "[car] expected 1 argument, given " <> show (length s)

toSchemeVal root "cdr" = BuiltInFunc f where
                        f [Pair (h,t)] = Right t
                        f [x] = Left "[car] can only be applied to List"
                        f s = Left $ "[car] expected 1 argument, given " <> show (length s)
toSchemeVal root "number?" = BuiltInFunc f where
                        f [Const (Num a)] = Right $ Const $ Bool True
                        f _ = Right $ Const $ Bool False
toSchemeVal root "*"  = BuiltInFunc $ \args -> do
                        a <- forM args (\e -> case e of
                                Const (Num i) -> Right i
                                _ -> Left "[+] can only be applied between numbers")
                        Right $ Const $ Num $ product' a 
toSchemeVal root "/" = None
toSchemeVal root "=" = None
toSchemeVal root "<" = None
toSchemeVal root "<=" = None
toSchemeVal root ">" = None
toSchemeVal root ">=" = None
toSchemeVal root "null?" = None
toSchemeVal root "pair?" = None
toSchemeVal root "list?" = None
toSchemeVal root "symbol?" = None
toSchemeVal root "cons" = None
toSchemeVal root "list" = None
toSchemeVal root "length" = None
toSchemeVal root "memq" = None
toSchemeVal root "last" = None
toSchemeVal root "append" = None
toSchemeVal root "set-car!" = None
toSchemeVal root "set-cdr!" = None
toSchemeVal root "boolean?" = None
toSchemeVal root "not" = None
toSchemeVal root "string?" = None
toSchemeVal root "string-append" = None
toSchemeVal root "symbol->string" = None
toSchemeVal root "string->symbol" = None
toSchemeVal root "string->number" = None
toSchemeVal root "number->string" = None
toSchemeVal root "procedure?" = None
toSchemeVal root "eq?" = None
toSchemeVal root "neq?" = None
toSchemeVal root "equal?" = None
toSchemeVal _ _ = undefined

                                                            
sum' :: [Number] -> Number
sum' = foldl' f (Integer 0) where
    f NaN _ = NaN
    f _ NaN = NaN
    f (Integer acc) (Integer i) = Integer (acc + i)

product' :: [Number] -> Number
product' = foldl' f (Integer 1) where
    f NaN _ = NaN
    f _ NaN = NaN
    f (Integer acc) (Integer i) = Integer (acc * i)

rootFrame :: Env
rootFrame = let root = defRootFrame root
                defRootFrame env = Frame 
                                (Mp.fromList ((\s -> (s, toSchemeVal env s)) 
                                    <$> ["+","-","car", "cdr","*","/","=","<","<=",">",">=",
                                        "number?","null?","pair?","list?","symbol?","cons","list",
                                        "length","memq","last","append","set-car!","set-cdr!",
                                        "boolean?","not","string?","string-append","symbol->string",
                                        "string->symbol","string->number","number->string","procedure?",
                                        "eq?","neq?","equal?" ])) 
                                NilFrame
            in root

findVal :: Env -> String -> Maybe SchemeVal
findVal NilFrame  s = Nothing
findVal (Frame values NilFrame) s = Mp.lookup s values
findVal (Frame values f ) s = case Mp.lookup s values of
                Just v -> Just v
                Nothing -> findVal f s

exist s mp = case Mp.lookup s mp of
    Nothing -> False
    Just _ -> True

setVal :: Env -> String -> SchemeVal -> Maybe Env
setVal NilFrame _ _ = Nothing
setVal (Frame vars par) s val = if exist s vars 
                                    then Just $ Frame (Mp.insert s val vars) par
                                    else setVal par s val


evalInput :: Env -> Toplevel -> Either String (SchemeVal, Env)
evalInput env top = run (eval top) env

newtype EResult a = EResult {run :: Env -> Either String (a, Env)}
instance Functor EResult where
    fmap f (EResult s) = EResult $ \e -> do
        (a, env) <- s e
        return (f a, env)

instance Applicative EResult where
    pure a = EResult $ \e -> Right (a, e)
    EResult ab <*> EResult a = EResult $ \env1 -> do
        (ab', env2) <- ab env1
        (a' , env3) <- a env2
        return (ab' a', env3)

instance Monad EResult where
    return a = EResult $ \e -> Right (a,e)
    EResult a >>= ab = EResult $ \env1 -> do
        (a',env2) <- a env1
        let (EResult b) = ab a'
        (b', env3) <- b env2
        return (b',env3)

efail :: String -> EResult a
efail s = EResult $ \_ -> Left s 

getEnv :: EResult Env
getEnv = EResult $ \e -> Right (e,e)

setEnv :: Env -> EResult ()
setEnv e = EResult $ \e2 -> Right ((),e)

class Eval a where
    eval :: a -> EResult SchemeVal

instance Eval Toplevel where
    eval (TopExp exp)= eval exp
    eval (TopDefine def) = eval def
    eval (Load fname) = error "load must be defined in main"

instance Eval Define where
    eval (DefVar (Id id) exp) = do 
        val <- eval exp
        env <- getEnv
        case env of
            NilFrame -> error "nilframe was given as environment[Define2]"
            (Frame vars par) -> do
                setEnv $ Frame (Mp.insert id val vars) par
                return None

    eval (DefFn (Id id) arg body) = do
        env <- getEnv
        case env of
            NilFrame -> error "nilframe was given as environment[Define2]"
            Frame vars par -> do
                let closure = Closure (env,(arg, body))
                let newVars = Mp.insert id closure vars
                setEnv $ Frame newVars par
                return None
-- envを指していたフレームは？
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
                                setEnv $ Frame (Mp.fromList params_args) parEnv
                                forM_ defs eval
                                res <- forM fnbody eval
                                setEnv env
                                return $ last res
                         
                        -- (define (f y) (define x y) (define z x) (+ x y z)) これがエラーにならない
                    
            e ->  efail $ show e <> "is not a procedure"
    eval (Set (Id s) exp) = do
        val <- eval exp
        env <- getEnv
        case setVal env s val of
            Just env' -> do
                setEnv env'
                return None
            Nothing -> efail $ "could not find value [" <> s <>"]"
    eval (Quote sexp) = eval sexp
    eval a = efail $ show a <> " is not defined" 

instance Eval SExp where
    eval (SConst c) = return $ Const c
    eval (SId id) = return $ Sym id
    eval (SList exps mexp) = do
        vals <- forM exps eval
        case mexp of
            Nothing -> return $ listToPair vals Nothing
            Just t -> do
                tail <- eval t
                return $ listToPair vals (Just tail)

        
fromId (Id s) = s

zipArgs :: [Id] -> Maybe Id -> [SchemeVal] -> Either [Char] [(Str, SchemeVal)]
zipArgs params Nothing args = if length args /= length params 
                                    then Left $ "not enough argument : expected " <> show (length params) <> ", given " <> show (length args)
                                    else Right $ zip (fromId <$> params) args

zipArgs params (Just (Id id)) args = if length args < length params 
                                    then Left $ "not enough argument : expected " <> show (length params) <> " or more, given " <> show (length args)
                                    else let (args', rest) = splitAt (length params) args 
                                            in Right $ (id, listToPair rest Nothing) : zip (fromId <$> params) args
listToPair [] (Just t) = t
listToPair [] Nothing = Const Nil
listToPair (x:xs) t = Pair (x, listToPair xs t)
instance Eval Id where
    eval (Id id) = do
        env <- getEnv
        case findVal env id of
            Just v -> return v
            Nothing -> efail $ "could not find value [" <> id <> "]\n" <> "note : env is \n" <> tostr 0 env 


instance Eval Const where
    eval c = return $ Const c