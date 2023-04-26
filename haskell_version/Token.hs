module Token where
import qualified Data.Text as T
import qualified Data.Map as Mp
import Pretty (TextDetails(PStr))

listToPair :: [Ptr] -> Maybe Ptr -> CResult Ptr
listToPair [] (Just t) = return t
listToPair [] Nothing = alloc PNil
listToPair (x:xs) t = do
    tail <- listToPair xs t
    alloc $ Pair (x, tail)
data Toplevel = TopExp !Exp | TopDefine !Define | Load !String 
data Define = DefVar !Id !Exp 
            | DefFn !Id !Params !Body
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
         | FnCall !Exp ![Exp] 
data Binding = Binding !Id !Exp 
data Branch = Branch !Exp ![Exp] 
data Body = Body ![Define] ![Exp] 
data Params = Params ![Id] !(Maybe Id) 
data SExp = SConst !Const | SId !Id | SList ![SExp] !(Maybe SExp) 
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

data SchemeVal =  PNum !Number | PBool !Bool | PString !String | PNil
                | Pair !(Ptr, Ptr)
                | Sym !Id
                | Closure !(Env,Lambda) 
                | BuiltInFunc !Func
                | None
type Lambda = (Params, Body)
type Func = [Ptr] -> CResult Ptr

scShow :: SchemeVal -> CResult String
scShow (PNum n) = return $ show n
scShow (PBool b) = return $ if b then "#t" else "#f"
scShow (PString s) = return s
scShow PNil = return "()"
scShow (Pair (car,cdr)) = showList' <$> pairToList (car,cdr)
         
scShow (Sym (Id id)) = return id
scShow (Closure _) = return "#<procedure>"
scShow (BuiltInFunc _) = return "#<procedure>"
scShow None = return "(none)"

pairToList :: (Ptr, Ptr) -> CResult ([String], Maybe String)
pairToList (car, cdr) = do
            carv <- valof car
            cdrv <- valof cdr
            cars <- scShow carv
            case cdrv of
                PNil -> return ([cars], Nothing)
                Pair p -> do
                    (vals,mval) <- pairToList p
                    return (cars:vals,mval)
                a -> do
                    as <- scShow a
                    return ([cars] ,Just as)

showList' :: ([String], Maybe String) -> String
showList' ([],Nothing) = "()"
showList' ([],Just v) = error "[showList]"
showList' (x:xs, Nothing) = "(" <> x <> foldl (\acc e -> acc <> " " <> e) "" xs <> ")"
showList' (x:xs, Just t) = "(" <> x <> foldl (\acc e -> acc <> " " <> e) "" xs <> " . " <> t <> ")"

type Ptr = Int
type Variables = Mp.Map String Ptr
data Env = Frame !Variables !Env | NilFrame deriving Show

insertVar _ _ NilFrame = error "cannot insert var in nilframe"
insertVar s p (Frame vars par) = Frame (Mp.insert s p vars) par


getVar s NilFrame = Left $ "could not find value " <> s 
getVar s (Frame vars par) = case (Mp.!?) vars s of
        Just v -> Right v
        Nothing -> getVar s par


type Heap = (Mp.Map Ptr SchemeVal, Ptr)

refof :: String -> CResult (Maybe Ptr)
refof s = CResult $ \ctx@(Ctx heap env) -> 
            let find NilFrame s = Nothing
                find (Frame vars par) s = case vars Mp.!? s of
                        Nothing -> find par s
                        Just v -> Just v
            in return (find env s , ctx)


valof :: Ptr -> CResult SchemeVal
valof p = CResult $ \ctx@(Ctx (heap,top) env) -> Right (heap Mp.! p,ctx)
            
alloc :: SchemeVal -> CResult Ptr
alloc v = CResult $ \(Ctx (heap,head) env) 
                -> Right (head+1, Ctx (Mp.insert (head+1) v heap, head+1) env)
def :: String -> Ptr -> CResult ()
def s p = CResult $ \ctx -> Right ((),f ctx) 
        where f (Ctx heap NilFrame) = error "[def] nilframe was given"
              f (Ctx heap (Frame vars par)) = Ctx heap $ Frame (Mp.insert s p vars) par
overWrite :: String -> Ptr  -> CResult ()
overWrite s p = CResult $ \(Ctx heap env) -> 
                let f NilFrame = Left $ "[assign] could not find value" <> s
                    f (Frame vars par) = case vars Mp.!? s of
                            Just _ -> Right $ Frame (Mp.insert s p vars) par
                            Nothing -> Frame vars <$> f par
                in do
                    newEnv <- f env
                    Right ((), Ctx heap newEnv)


data Ctx = Ctx {heap:: !Heap, env:: !Env} 
newtype CResult a = CResult {run :: Ctx -> Either String (a, Ctx)}
instance Functor CResult where
    fmap f (CResult s) = CResult $ \e -> do
        (a, ctx) <- s e
        return (f a, ctx)

instance Applicative CResult where
    pure a = CResult $ \e -> Right (a, e)
    CResult ab <*> CResult a = CResult $ \ctx1 -> do
        (ab', ctx2) <- ab ctx1
        (a' , ctx3) <- a ctx2
        return (ab' a', ctx3)

instance Monad CResult where
    return a = CResult $ \e -> Right (a,e)
    CResult a >>= ab = CResult $ \c1 -> do
        (a',c2) <- a c1
        let (CResult b) = ab a'
        (b', c3) <- b c2
        return (b',c3)

efail :: String -> CResult a
efail s = CResult $ \_ -> Left s 

getEnv :: CResult Env
getEnv = CResult $ \c -> Right (env c,c)

setEnv :: Env -> CResult ()
setEnv e = CResult $ \c -> Right ((), Ctx (heap c) e)
