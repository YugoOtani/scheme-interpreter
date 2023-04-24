module Token where
import qualified Data.Text as T
import qualified Data.Map as Mp

pairToList (car, cdr) = case cdr of
    Pair p -> car:pairToList p
    s -> car:[s]
listToPair [] (Just t) = t
listToPair [] Nothing = Const Nil
listToPair (x:xs) t = Pair (x, listToPair xs t)
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
instance Eq Number where
    NaN == _ = False
    _ == NaN = False
    Integer i == Integer j = i == j
instance Show Number where
    show NaN = "NaN"
    show (Integer i) = show i
instance Show Const where
    show (Num i) = show i
    show (Bool b) = if b then "#t" else "#f"
    show (String s) =  "\"" <> s <> "\""
    show Nil = "()"
newtype Id = Id String deriving (Show,Eq)

data SchemeVal = Const !Const 
                | Pair !Pair
                | Sym !Id
                | Closure !(Env,Lambda) 
                | BuiltInFunc !Func
                | None 
type Pair = (SchemeVal ,SchemeVal)
type Lambda = (Params, Body)
type Func = [SchemeVal] -> ReturnVal
type ReturnVal = Either String SchemeVal

instance Show SchemeVal where
    show (Const c) = show c
    show (Pair p) = "(" <> showSchemeList (pairToList p) <> ")"
    show (Sym (Id s)) = s
    show (Closure _) = "#<procedure>"
    show (BuiltInFunc _) = "#<procedure>"
    show None = "(none)" 

showSchemeList [] = ""
showSchemeList (v:[Const Nil]) = show v
showSchemeList [v,v2] = show v <> " . " <> show v2
showSchemeList (car:cdr) = show car <> " " <> showSchemeList cdr

type Variables = Mp.Map String SchemeVal
data Env = Frame !Variables !Env | NilFrame deriving Show
