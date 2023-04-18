{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
import Parser
import TokenParser
import Token
import qualified Data.Map as Mp
import Control.Monad (forM, forM_)
import Data.List

toSchemeVal root "+"  = Closure (root, (Arg [] (Just (Id "a")) ,def))
    where def args = Const . Num . sum <$> forM args (\e -> case e of
                                                    Const (Num i) -> Right i
                                                    _ -> Left "'+' can only be applied between numbers")
toSchemeVal _ _ = undefined

rootFrame :: Env
rootFrame = let root = defRootFrame root
                defRootFrame env = Frame (Mp.fromList ((\s -> (s,toSchemeVal env s)) <$> ["+"])) NilFrame
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
            putStr $ "Haskeme" <> show i <> "] > "
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
    eval env (FnCall fn params) = do
        (res, _) <- eval env fn --env?
        params' <-  map fst <$> forM params (eval env)
        case res of 
            Closure (parEmv, (Arg ids mid, func)) -> undefined
            e ->  Left $ show e <> "is not a procedure"

    eval env _ = undefined

instance Eval Id where
    eval env (Id id) = case findVal env id of
        Just v -> Right (v,env)
        Nothing -> Left $ "could not find value [" <> id <> "]"


instance Eval Const where
    eval env c = Right (Const c, env)