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
import Control.Exception
import System.IO
import Control.DeepSeq


main = do
    loop 1 initialCtx where
        loop i env = do
            putStr $ "Haskeme[" <> show i <> "] > "
            input <- getLine;
            if input == "quit" 
                then do
                    putStrLn "goodbye"
                    return ()
                else do
                    newEnv <- exec env input
                    loop (i+1) newEnv
                    
            `catch` \e -> do
                putStrLn $ displayException (e::IOException) 
                loop (i+1) env
            

exec :: Ctx -> String -> IO Ctx 
exec ctx input = case parse input of
    Right (Load fname) -> do
        input <- withFile fname ReadMode $ \fd -> do
                        content <- hGetContents fd
                        return $!! content
                                                        

        case parseMany input of
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
                            putStrLn $ tostr 0 parseRes
                            putStrLn r 
                            return newCtx
    Left l -> do
        putStrLn "parse error"
        return ctx

zeroCtx = Ctx (Mp.empty,0) (Frame Mp.empty NilFrame)
initialCtx :: Ctx
initialCtx = case forM_ rootFn (\s -> alloc (globFn s) >>= \p -> def s p) `run` zeroCtx of
            Left l -> error $ "[init ctx error]" <> l
            Right (_,ctx) -> ctx

