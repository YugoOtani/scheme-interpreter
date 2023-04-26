{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module SchemeFunc where
import Control.Monad
import Token
import Data.List
import Data.Maybe


scPlus args = do
        a <- forM args (\e -> case e of
            PNum i -> Right i
            _ -> Left "[+] can only be applied between numbers")
        Right $ PNum $ sum' a
scMinus = f where
        f [] = Left "[-] must have 1 argument"
        f (PNum NaN : xs) = Right $ PNum NaN
        f (PNum (Integer i) : xs) = do
            a <- forM xs (\e -> case e of
                PNum i -> Right i
                _ -> Left "[-] can only be applied between numbers")
            let ans = case sum' a of
                        NaN -> NaN
                        (Integer i2) -> Integer (i - i2)
            Right $ PNum ans
        f _ = Left "'-' can only be applied between numbers"
scCar = BuiltInFunc $ \args -> case args of
                        [p] -> do
                            v <- valof p
                            case v of
                                (Pair (car,cdr)) -> return car
                                _ -> efail "[car] expected pair"
                        v -> efail  "[car] number of argument is incorrect"

scCdr = BuiltInFunc $ \args -> case args of
                        [p] -> do
                            v <- valof p
                            case v of
                                (Pair (car,cdr)) -> return cdr
                                _ -> efail "[car] expected pair"
                        v -> efail  "[car] number of argument is incorrect"
scPNumber = f where
                        f [PNum a] = Right $ PBool True
                        f _ = Right $ PBool False
scMult args = do
                        a <- forM args (\e -> case e of
                                PNum i -> Right i
                                _ -> Left "[+] can only be applied between numbers")
                        Right $ PNum $ product' a
scDiv args = do
                        a <- forM args (\e -> case e of
                                PNum i -> Right i
                                _ -> Left "[+] can only be applied between numbers")
                        case a of
                            [] -> Left "[+] one or more argument is given"
                            (x:xs) -> Right $ PNum $ div' x xs
scNumEq = comp (==)
scLt = comp (<)
scLeq = comp (<=)
scGt = comp (>)
scGeq = comp (>=)
scNull args = case args of
                            [PNil] -> Right $ PBool True
                            [_] -> Right $ PBool False
                            s -> Left $ "Number of argument is incorrect. Expected 1, given " <> show (length s)
scPPair args = case args of
                            [Pair _] -> Right $ PBool True
                            [_] -> Right $ PBool False
                            s -> Left $ "Number of argument is incorrect. Expected 1, given " <> show (length s)
scPList = None
scSym args = case args of
                            [Sym _] -> Right $ PBool True
                            [_] -> Right $ PBool False
                            s -> Left $ "Number of argument is incorrect. Expected 1, given " <> show (length s)
scCons = BuiltInFunc $ \args -> case args of
                            [car, cdr] -> alloc $ Pair (car,cdr)
                            s -> efail $ "Number of argument is incorrect. Expected 2, given " <> show (length s)
scList = None
scLen = None
scMemq = None
scLast = None
scAppend = None
scSetCar = None
scSetCdr = None
scPBool = None
scNot = None
scPStr = None
scStrAppend = None
scSymToStr = None
scStrToSym = None
scStrToNum = None
scNumToStr = None
scPProc = None
scEq = None
scNeq = None
scEqual = None


memq elem (Pair (x, xs)) = undefined

eq' val1 val2 = undefined
equal' val1 val2 = undefined
comp op args = case args of
                            [PNum NaN, _] -> Right $ PBool False
                            [_, PNum NaN] -> Right $ PBool False
                            [PNum (Integer i), PNum (Integer j)] -> Right $ PBool (op i j)
                            [_,_] -> Left "[<] can only be applied between numbers"
                            s -> Left $ "Number of argument is incorrect. Expected 2, given " <> show (length s)

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

div' :: Number -> [Number] -> Number
div' x xs = case (x, product' xs) of
    (NaN, _) -> NaN
    (_, NaN) -> NaN
    (_, Integer 0) -> NaN
    (Integer x, Integer y) -> Integer (x `div` y)