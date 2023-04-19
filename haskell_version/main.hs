{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
import Parser
import TokenParser
import Token
import qualified Data.Map as Mp
import Control.Monad (forM, forM_)
import Data.List

toSchemeVal :: Env -> [Char] -> SchemeVal
toSchemeVal root "+"  = BuiltInFunc $ \args -> do
                        a <- forM args (\e -> case e of
                                Const (Num i) -> Right i
                                _ -> Left "'+' can only be applied between numbers")
                        Right $ Const $ Num $ sum' a 
toSchemeVal root "-" = BuiltInFunc f where
                        f [] = Left "'-' must have 1 argument"
                        f (Const (Num NaN) : xs) = Right $ Const $ Num NaN
                        f (Const (Num (Integer i)) : xs) = do
                            a <- forM xs (\e -> case e of
                                Const (Num i) -> Right i
                                _ -> Left "'-' can only be applied between numbers")
                            let ans = case sum' a of
                                        NaN -> NaN
                                        (Integer i2) -> Integer (i - i2)
                            Right $ Const $ Num ans
                        f _ = Left "'-' can only be applied between numbers"

toSchemeVal _ _ = undefined

                                                            
sum' :: [Number] -> Number
sum' = foldl' f (Integer 0) where
    f NaN _ = NaN
    f _ NaN = NaN
    f (Integer acc) (Integer i) = Integer (acc + i)


rootFrame :: Env
rootFrame = let root = defRootFrame root
                defRootFrame env = Frame (Mp.fromList ((\s -> (s, toSchemeVal env s)) <$> ["+","-"])) NilFrame
            in root

findVal :: Env -> String -> Maybe SchemeVal
findVal NilFrame  s = Nothing
findVal (Frame values NilFrame) s = Mp.lookup s values
findVal (Frame values f ) s = case Mp.lookup s values of
                Just v -> Just v
                Nothing -> findVal f s


main = do
    loop 1 rootFrame where
        loop i env = do
            putStr $ "Haskeme[" <> show i <> "] > "
            input <- getLine
            case parse input of
                Right parseRes -> case eval env parseRes of
                    Left l -> do
                        putStrLn $ tostr 0 parseRes
                        putStrLn l
                        loop (i+1) env
                    Right (r,newEnv) -> do
                        putStrLn $ tostr 0 parseRes
                        print r   -- TODO:defineなどreturn typeがないとき
                        putStrLn $ "env:\n" <> tostr 0 newEnv
                        loop (i+1) newEnv
                Left _ -> do
                    putStrLn "parse error"
                    loop (i+1) env
                

evalInput :: Env -> Toplevel -> Either String (SchemeVal, Env)
evalInput = eval


class Eval a where
    eval :: Env -> a -> Either String (SchemeVal,Env)

instance Eval Toplevel where
    eval env (TopExp exp)= eval env exp
    eval env (TopDefine def) = eval env def
    eval env (Load fname) = undefined

instance Eval Define where
    eval NilFrame _ = error "nilframe was given as environment[Define1]"
    eval env (DefVar (Id id) exp) = do -- env env2...?
        (val,env2) <- eval env exp
        case env2 of
            NilFrame -> error "nilframe was given as environment[Define2]"
            (Frame vars cld) -> let newVars = Mp.insert id val vars in 
                                return (Const Nil,Frame newVars cld) -- ?
    eval env@(Frame vars cld ) (DefFn (Id id) arg body) = do
        let closure = Closure (env,(arg, body))
        let newVars = Mp.insert id closure vars
        return (Const Nil, Frame newVars cld)
-- envを指していたフレームは？
instance Eval Exp where
    eval env (ExpId id) = eval env id
    eval env (ExpConst c) = eval env c
    eval env (FnCall fn args) = do
        (res, env') <- eval env fn --env?
        (args',env'') <-  evalList env' args
        case res of 
            BuiltInFunc f -> do
                    ret <- f args'
                    return (ret, env'')
            Closure (parEnv,(Params ids mid, Body defs exps)) ->   -- evaluate defs -> exps
                    if null exps then error "body has no exp" -- see TokenParser.hs.pbody
                    else do
                        params_args <- Mp.fromList <$> zipArgs ids mid args'
                        let newEnv = Frame params_args parEnv
                        (_, newEnv') <- evalList newEnv defs
                        (expres, newEnv'') <- evalList newEnv' exps
                        return (last expres, env) -- envだと副作用がなくなる
                    
                    
            e ->  Left $ show e <> "is not a procedure"
    eval env a = Left $ show a <> " is not defined" 

evalList ::  Eval a => Env -> [a] -> Either String ([SchemeVal],Env)
evalList env [] = Right ([],env) 
evalList env (x:xs) = do
    (xval,env') <- eval env x
    (xsval,env'') <- evalList env' xs
    return (xval:xsval,env'')
fromId (Id s) = s

zipArgs :: [Id] -> Maybe Id -> [SchemeVal] -> Either [Char] [(Str, SchemeVal)]
zipArgs params Nothing args = if length args /= length params 
                                    then Left $ "not enough argument : expected " <> show (length params) <> ", given " <> show (length args)
                                    else Right $ zip (fromId <$> params) args

zipArgs params (Just (Id id)) args = if length args < length params 
                                    then Left $ "not enough argument : expected " <> show (length params) <> " or more, given " <> show (length args)
                                    else let (args', rest) = splitAt (length params) args 
                                            in Right $ (id, List rest) : zip (fromId <$> params) args

instance Eval Id where
    eval env (Id id) = case findVal env id of
        Just v -> Right (v,env)
        Nothing -> Left $ "could not find value [" <> id <> "]\n" <> "note : env is \n" <> tostr 0 env 


instance Eval Const where
    eval env c = Right (Const c, env)