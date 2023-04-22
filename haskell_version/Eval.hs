module Eval where

import Token
import Control.Monad
import ToStr
import qualified Data.Map as Mp
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
        case findVal env id of
            Just v -> return v
            Nothing -> efail $ "could not find value [" <> id <> "]\n" <> "note : env is \n" <> tostr 0 env 


instance Eval Const where
    eval c = return $ Const c