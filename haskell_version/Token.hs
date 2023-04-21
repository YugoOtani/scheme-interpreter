module Token where
import qualified Data.Text as T
import qualified Data.Map as Mp
data Toplevel = TopExp !Exp | TopDefine !Define | Load !String deriving Show
data Define = DefVar !Id !Exp 
            | DefFn !Id !Params !Body deriving Show
data Exp = ExpConst !Const 
         | ExpId !Id
         | Lambda !Lambda
         | Quote !SExp
         | Set !Id !Exp
         | Let !(Maybe Id) ![Binding] !Body
         | Let2 ![Binding] !Body
         | LetRec ![Binding] !Body
         | If !Exp !Exp !(Maybe Exp)
         | Cond ![Branch] !(Maybe [Exp])
         | And ![Exp]
         | Or ![Exp]
         | Begin ![Exp]
         | FnCall !Exp ![Exp] deriving Show
data Binding = Binding !Id !Exp deriving Show
data Branch = Branch !Exp ![Exp] deriving Show
data Body = Body ![Define] ![Exp] deriving Show
data Params = Params ![Id] !(Maybe Id) deriving Show
data SExp = SConst !Const | SId !Id | SList ![SExp] !(Maybe SExp) deriving Show
data Const = Num !Number | Bool !Bool | String !String | Nil
data Number = Integer !Integer | NaN 
instance Show Number where
    show NaN = "NaN"
    show (Integer i) = show i
instance Show Const where
    show (Num i) = show i
    show (Bool b) = if b then "#t" else "#f"
    show (String s) =  "\"" <> s <> "\""
    show Nil = "()"
newtype Id = Id String deriving Show

data SchemeVal = Const !Const 
                | List ![SchemeVal] (Maybe SchemeVal)
                | Sym !Id
                | Closure !(Env,Lambda) 
                | BuiltInFunc !Func
                | None
type Lambda = (Params, Body)
type Func = [SchemeVal] -> ReturnVal
type ReturnVal = Either String SchemeVal
instance Show SchemeVal where
    show (Const c) = show c
    show (List [] (Just _)) = error "this pattern must be excluded in parse"
    show (List [] Nothing) = "()"
    show (List (x:xs) (Just y)) = "(" <> show x
                                      <> foldl (\acc e -> acc <> " " <> show e) "" xs
                                      <> " . " <> show y <> ")"
    show (List (x:xs) Nothing) = "(" <> show x
                                     <> foldl (\acc e -> acc <> " " <> show e) "" xs
                                     <> ")"
    show (Sym (Id s)) = s
    show (Closure _) = "#<procedure>"
    show (BuiltInFunc _) = "#<procedure>"
    show None = "(none)" 


type Variables = Mp.Map String SchemeVal
data Env = Frame !Variables !Env | NilFrame deriving Show
instance ToStr Env where
    tostr i NilFrame = indentN i <> "Nil"
    tostr i (Frame vars par) = indentN i <> show vars <> "\n" <> tostr (i+1) par

indentStr = " |"
indentN 0 = indentStr
indentN i = indentStr <> indentN (i-1)
showChild i cld = indentN i <> show cld
class ToStr a where
    tostr :: Int -> a -> String

instance ToStr Toplevel where
    tostr i (TopExp exp) = unlines1 [indentN i <> "Top(Exp)",tostr (i+1) exp]
    tostr i (TopDefine def) = unlines1 [indentN i <> "Top(Def)", tostr (i+1) def]
    tostr i (Load str) = indentN i<> "Top(Load) [" <> str <> "]"
instance ToStr Define where
    tostr i (DefVar id exp) = unlines1 [indentN i <> "DefVar"
                                            ,showChild (i+1) id
                                            , tostr (i+1) exp]
    tostr i (DefFn id prms body) = unlines1 [indentN i <> "DefFn", showChild (i+1) id, tostr (i+1) prms, tostr (i+1) body]

instance ToStr Exp where
    tostr i (ExpConst c) = unlines1 [indentN i <> "Exp(Const)", tostr i c]
    tostr i (ExpId id) = unlines1 [indentN i <> "Exp(Id)", showChild (i+1) id]
    tostr i (Lambda (Params ids id,body)) = unlines1 [indentN i <> "Exp(Lambda)" 
                                                    ,indentN (i+1) <> "Args"
                                                    ,indentN (i+2) <> show ids <> " " <> show id
                                                    , tostr (i+1) body]

    tostr i (Quote sexp) = unlines1 [indentN i <> "Exp(SExp)" , tostr (i+1) sexp]
    tostr i (FnCall exp exps) = unlines1 [indentN i <> "Exp(FnCall1)"
                                        , tostr (i+1) exp, tostr (i+1) exps]

    tostr i (Set id exp) = unlines1 [indentN i <> "Exp(Set)", indentN (i+1) <> show id, tostr (i+1) exp]
    tostr i (Let id binds body) = unlines1 [indentN i <> "Let", indentN (i+1) <> show id, tostr (i+1) binds,tostr (i+1) body]
    tostr i (Let2 binds body) = unlines1 [indentN i <> "Let2", tostr (i+1) binds, tostr (i+1) body]
    tostr i (LetRec binds body )  = unlines1 [indentN i <> "LetRec", tostr (i+1) binds, tostr (i+1) body]
    tostr i (If exp1 exp2 exp3) = unlines1 [indentN i <> "If", tostr (i+1) exp1, tostr (i+1) exp2, tostr (i+1) exp3]
    tostr i (Cond branch exp) = unlines1 [indentN i <> "Cond", tostr (i+1) branch, tostr (i+1) exp]
    tostr i (And exp) = unlines1 [indentN i <> "And", tostr (i+1) exp]
    tostr i (Or exp) = unlines1 [indentN i <> "Or", tostr (i+1) exp]
    tostr i (Begin exp) = unlines1 [indentN i <> "Begin", tostr (i+1) exp]

instance ToStr Binding where
    tostr i (Binding id exp) = unlines1 [indentN i <> "Binding " <> show id, tostr (i+1) exp]
instance ToStr SExp where
    tostr i (SConst c ) = unlines1 [indentN i <> "SExp(Const)", tostr (i+1) c]
    tostr i (SId id ) = unlines1 [indentN i <> "SExp(Id)" , indentN (i+1) <> show id]
    tostr i (SList sexps sexp) = unlines1 [indentN i <> "SExp(List)"
                                            ,tostr (i+1) sexps
                                            ,tostr (i+1) sexp]
instance ToStr Branch where
    tostr i (Branch cond opr) = unlines1 [indentN i <> "Branch"
                                        ,indentN (i+1) <> "cond"
                                        ,tostr (i+1) cond
                                        ,indentN (i+1) <> "operation"
                                        ,tostr (i+1) opr]
instance ToStr Params where
    tostr i (Params args rest) = unlines1 [indentN i <> "Args"
                                        , indentN (i+1) <> show args
                                        , indentN (i+1) <> show rest]

instance ToStr Const where
    tostr i (Num n) = indentN i <> "Const " <> show n
    tostr i (Bool b) = indentN i <> "Const " <>show b
    tostr i (String s) = indentN i <> "Const " <> s
    tostr i Nil = indentN i <> "Const " <> "()"
instance ToStr Body where
    tostr i (Body defs exps) = unlines1 [indentN i <> "Body" 
                                ,indentN (i+1) <> "Defines"
                                ,tostr (i+2) defs
                                ,indentN (i+1) <> "Exps"
                                ,tostr (i+2) exps]

instance ToStr a => ToStr [a] where
    tostr i [] = indentN i <> "empty"
    tostr i as = indentN i <> "List\n" <> unlines1 (tostr (i+1) <$> as)
instance ToStr a => ToStr (Maybe a) where
    tostr i Nothing = indentN i <> "Nothing"
    tostr i (Just v) = indentN i <> "Just\n" <> tostr (i+1) v 

unlines1 :: [String] -> String
unlines1 [] = ""
unlines1 [s] = s
unlines1 (s:xs) = s <> "\n" <> unlines1 xs 