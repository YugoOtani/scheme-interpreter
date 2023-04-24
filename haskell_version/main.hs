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
    loop 1 initialCtx where
        loop i env = do
            putStr $ "Haskeme[" <> show i <> "] > "
            input <- getLine
            newEnv <- exec env input
            loop (i+1) newEnv

exec :: Ctx -> String -> IO Ctx 
exec ctx input = case parse input of
    Right (Load fname) -> do
        s <- readFile fname
        case parseMany s of
            Left err -> do
                putStrLn err
                return ctx
            Right tops -> do
                case run (forM_ tops eval) ctx of
                    Left err -> do
                        putStrLn err
                        return ctx
                    Right (_, newCtx) -> do
                        putStrLn "[load] success"
                        return newCtx

    Right parseRes -> case evalInput parseRes ctx of
                        Left l -> do
                            putStrLn $ tostr 0 parseRes
                            putStrLn l
                            return ctx
                        Right (r,newCtx) -> do
                            -- putStrLn $ tostr 0 parseRes
                            print r 
                            -- putStrLn $ "env:\n" <> tostr 0 newEnv
                            return newCtx
    Left l -> do
        putStrLn $ "parse error" <> l
        return ctx



