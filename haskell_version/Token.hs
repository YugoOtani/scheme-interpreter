module Token where
import qualified Data.Text as T
data Toplevel = TopExp !Exp | TopDefine !Define | Load !String deriving Show
data Define = DefVar !Id !Exp 
            | DefFn !Id !Params !Body deriving Show
data Params = Params ![Id] !(Maybe Id) deriving Show
data Exp = ExpConst !Const 
         | ExpId !Id
         | Lambda !Arg !Body
         | Quote !SExp
         | List1 !Id ![Exp]
         | List2 !Exp ![Exp] deriving Show
         
data Branch = Branch !Exp ![Exp] deriving Show
data Body = Body ![Define] ![Exp] deriving Show
data Arg = Arg ![Id] !(Maybe Id) deriving Show
data SExp = SConst !Const | SId !Id | SList ![SExp] !(Maybe SExp) deriving Show
data Const = Num !Integer | Bool !Bool | String !String | Nil deriving Show
newtype Id = Id String deriving Show





indentN 0 = "  "
indentN i = "  " <> indentN (i-1)
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

instance ToStr Params where
    tostr i (Params ids mid) = indentN i <> "Params" <> show ids <> ", " <> show mid
instance ToStr Exp where
    tostr i (ExpConst c) = unlines1 [indentN i <> "Exp(Const)", tostr i c]
    tostr i (ExpId id) = unlines1 [indentN i <> "Exp(Id)", showChild (i+1) id]
    tostr i (Lambda (Arg ids id) body) = unlines1 [indentN i <> "Exp(Lambda)" 
                                                    ,indentN (i+1) <> "Args"
                                                    ,indentN (i+2) <> show ids <> " ", show id
                                                    , tostr (i+1) body]

    tostr i (Quote sexp) = unlines1 [indentN i <> "Exp(SExp)" , tostr (i+1) sexp]
    tostr i (List1 id exps) = unlines1 [indentN i <> "Exp(FnCall1)"
                                        , indentN (i+1) <> show id, tostr (i+1) exps]
    tostr i (List2 exp1 exp2) = unlines1 [indentN i <> "Exp(FnCall2)"
                                        , tostr (i+1) exp1
                                        , tostr (i+1) exp2]

instance ToStr SExp where
    tostr i (SConst c ) = unlines1 [indentN i <> "SExp(Const)", tostr (i+1) c]
    tostr i (SId id ) = unlines1 [indentN i <> "SExp(Id)" , show id]
    tostr i (SList sexps sexp) = unlines1 [indentN i <> "SExp(List)"
                                            ,tostr (i+1) sexps
                                            ,tostr (i+1) sexp]
instance ToStr Branch where
    tostr i (Branch cond opr) = unlines1 [indentN i <> "Branch"
                                        ,indentN (i+1) <> "cond"
                                        ,tostr (i+1) cond
                                        ,indentN (i+1) <> "operation"
                                        ,tostr (i+1) opr]
instance ToStr Arg where
    tostr i (Arg args rest) = unlines1 [indentN i <> "Args"
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