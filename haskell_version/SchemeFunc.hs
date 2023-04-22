{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module SchemeFunc where
import Control.Monad
import Token
import Data.List


scPlus = BuiltInFunc $ \args -> do
                        a <- forM args (\e -> case e of
                                Const (Num i) -> Right i
                                _ -> Left "[+] can only be applied between numbers")
                        Right $ Const $ Num $ sum' a 
scMinus = BuiltInFunc f where
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
scCar = BuiltInFunc f where
                        f [Pair (h,t)] = Right h
                        f [x] = Left "[car] can only be applied to List"
                        f s = Left $ "[car] expected 1 argument, given " <> show (length s)

scCdr = BuiltInFunc f where
                        f [Pair (h,t)] = Right t
                        f [x] = Left "[car] can only be applied to List"
                        f s = Left $ "[car] expected 1 argument, given " <> show (length s)
scPNumber = BuiltInFunc f where
                        f [Const (Num a)] = Right $ Const $ Bool True
                        f _ = Right $ Const $ Bool False
scMult = BuiltInFunc $ \args -> do
                        a <- forM args (\e -> case e of
                                Const (Num i) -> Right i
                                _ -> Left "[+] can only be applied between numbers")
                        Right $ Const $ Num $ product' a 
scDiv  = BuiltInFunc $ \args -> do
                        a <- forM args (\e -> case e of
                                Const (Num i) -> Right i
                                _ -> Left "[+] can only be applied between numbers")
                        case a of
                            [] -> Left "[+] one or more argument is given"
                            (x:xs) -> Right $ Const $ Num $ div' x xs
scNumEq = comp (==)
scLt = comp (<)
scLeq = comp (<=)
scGt = comp (>)
scGeq = comp (>=)
scNull = BuiltInFunc $ \args -> case args of
                            [Const Nil] -> Right $ Const $ Bool True
                            [_] -> Right $ Const $ Bool False
                            s -> Left $ "Number of argument is incorrect. Expected 1, given " <> show (length s) 
scPPair = BuiltInFunc $ \args -> case args of
                            [Pair _] -> Right $ Const $ Bool True
                            [_] -> Right $ Const $ Bool False
                            s -> Left $ "Number of argument is incorrect. Expected 1, given " <> show (length s) 
scPList = BuiltInFunc $ \args -> case args of
                            [p] -> Right $ Const $ Bool $ isList p
                            s -> Left $ "Number of argument is incorrect. Expected 1, given " <> show (length s) 
scSym = BuiltInFunc $ \args -> case args of
                            [Sym _] -> Right $ Const $ Bool True
                            [_] -> Right $ Const $ Bool False
                            s -> Left $ "Number of argument is incorrect. Expected 1, given " <> show (length s) 
scCons = BuiltInFunc $ \args -> case args of
                            [car, cdr] -> Right $ Pair (car,cdr)
                            s -> Left $ "Number of argument is incorrect. Expected 2, given " <> show (length s) 
scList = BuiltInFunc $ \args -> Right (listToPair args Nothing)
scLen = BuiltInFunc $ \args -> case args of
                            [p] ->  Const . Num . Integer <$> length' p
                            s -> Left $ "Number of argument is incorrect. Expected 1, given " <> show (length s) 
scMemq = BuiltInFunc $ \args -> case args of
                            [elem,lst] -> undefined
                            s -> Left $ "Number of argument is incorrect. Expected 1, given " <> show (length s) 
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
isList (Pair (_, Const Nil)) = True
isList (Pair (_, cld)) = isList cld
isList _ = False

length' (Pair (_, Const Nil)) = Right 1
length' (Pair (_, p)) = (+) <$> Right 1 <*> length' p
length' _ = Left "[length] can only be applied to list"

memq elem (Pair (x, xs)) = undefined

comp op = BuiltInFunc $ \args -> case args of
                            [Const (Num NaN), _] -> Right $ Const $ Bool False
                            [_, Const (Num NaN)] -> Right $ Const $ Bool False
                            [Const (Num (Integer i)), Const (Num (Integer j))] -> Right $ Const $ Bool (op i j)
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