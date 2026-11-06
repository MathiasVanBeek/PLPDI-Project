import Data.Data (Data)


type Numb = Integer

newtype Loc = Loc Integer 
    deriving (Eq, Show, Data)

data AExp =
    ANum Numb |
    ALoc Loc |
    IMPAdd AExp AExp |
    IMPSub AExp AExp |
    IMPMul AExp AExp
    deriving (Eq, Show, Data)

data BExp =
    IMPFalse |
    IMPTrue |
    IMPEq AExp AExp |
    IMPLeq AExp AExp |
    IMPNeg BExp |
    IMPAnd BExp BExp |
    IMPOr BExp BExp
    deriving (Eq, Show, Data)

data Com = 
    Skip |
    IMPAssign Loc AExp |
    N Com Com |
    IMPIfThEl BExp Com Com|
    IMPWhile BExp Com
    deriving (Eq, Show, Data)

type State = Loc -> Numb

inititalState :: State
inititalState _ = 0

update :: State -> Loc -> Numb -> State
update s l n l' = if l==l' then n else s l

evalAExp :: State -> AExp -> Numb
evalAExp _ (ANum n) = n
evalAExp s (ALoc l) = s l
evalAExp s (IMPAdd a1 a2) = evalAExp s a1 + evalAExp s a2
evalAExp s (IMPSub a1 a2) = evalAExp s a1 - evalAExp s a2
evalAExp s (IMPMul a1 a2) = evalAExp s a1 * evalAExp s a2

evalBExp :: State -> BExp -> Bool
evalBExp _ IMPFalse = False
evalBExp _ IMPTrue = True
evalBExp s (IMPEq a1 a2) = (evalAExp s a1) == (evalAExp s a2)
evalBExp s (IMPLeq a1 a2) = (evalAExp s a1) <= (evalAExp s a2)
evalBExp s (IMPNeg b1) = not $ evalBExp s b1
evalBExp s (IMPAnd b1 b2) = (evalBExp s b1) && (evalBExp s b2)
evalBExp s (IMPOr b1 b2) = (evalBExp s b1) || (evalBExp s b2)

execCom :: State -> Com -> State
execCom s Skip = s
execCom s (IMPAssign l a) = update s l (evalAExp s a)
execCom s (N c1 c2) = execCom (execCom s c1) c2
execCom s (IMPIfThEl b c1 c2) = if evalBExp s b then execCom s c1 else execCom s c2
execCom s (IMPWhile b c) = if evalBExp s b then execCom (execCom s c) (IMPWhile b c) else s 

