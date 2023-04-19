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
            let res = evalInput input env
            case res of
                Left l -> do
                    putStrLn l
                    loop (i+1) env
                Right (r,newEnv) -> do
                    print r
                    loop (i+1) newEnv

evalInput :: String -> Env -> Either String (SchemeVal, Env)
evalInput s env = do
    res <- parse s
    eval env res


class Eval a where
    eval :: Env -> a -> Either String (SchemeVal,Env)

instance Eval Toplevel where
    eval env (TopExp exp)= eval env exp
    eval env _ = undefined

instance Eval Exp where
    eval env (ExpId id) = eval env id
    eval env (ExpConst c) = eval env c
    eval env (FnCall fn params) = do
        (res, _) <- eval env fn --env?
        params' <-  map fst <$> forM params (eval env)
        case res of 
            BuiltInFunc f -> do
                    ret <- f params'
                    return (ret, env)
            Closure (parEmv, (Arg ids mid, body)) -> do
                    undefined
            e ->  Left $ show e <> "is not a procedure"
    eval env a = Left $ show a <> " is not defined" 

instance Eval Id where
    eval env (Id id) = case findVal env id of
        Just v -> Right (v,env)
        Nothing -> Left $ "could not find value [" <> id <> "]"


instance Eval Const where
    eval env c = Right (Const c, env)