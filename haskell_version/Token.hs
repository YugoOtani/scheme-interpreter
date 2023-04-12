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
         | Set !Id !Exp
         | Let1 !(Maybe Id) ![Binding] !Body
         | Let2 ![Binding] !Body
         | LetRec ![Binding] !Body
         | If {cond:: !Exp, thenExp:: !Exp, elseExp:: !Exp}
         | ExpCond ![Cond] !(Maybe [Exp])
         | ExpAnd ![Exp]
         | ExpOr ![Exp]
         | Begin ![Exp] deriving Show
data Cond = Cond !Exp ![Exp] deriving Show
data Body = Body ![Define] ![Exp] deriving Show
data Arg = Arg ![Id] !(Maybe Id) deriving Show
data Binding = Binding !Id !Exp deriving Show
data SExp = SConst !Const | SId !Id | SExps ![SExp] !(Maybe SExp) deriving Show
data Const = Num !Integer | Bool !Bool | String !String | Nil deriving Show
newtype Id = Id String deriving Show