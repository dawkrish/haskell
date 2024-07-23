data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

-- A look up table with k as the type of keys an v as the type of values
-- eg: [(1, 'a'), (2, 'b')] :: Assoc Int Char
type Assoc k v = [(k,v)]
-- Assoc with keys(Char) & vals(Bool) 
-- eg [('A', True), ('B', False)] :: Subst 
-- It means A has value True, B has value False
type Subst = Assoc Char Bool

t = [('A', True), ('B', False)]

-- For a given Prop, it checks the result of truth tables of that props
-- if all true, then that prop is always true, thus a tautology
isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

-- Evalulate a prop for a given lookup table
eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval lookup (Var x) = head [b | (c, b) <- lookup, c == x]
eval lookup (Not p) = not (eval lookup p)
eval lookup (And p q) = (eval lookup p) && (eval lookup q)
eval lookup (Imply p q) = (eval lookup p) <= (eval lookup q)

--Find all the possible variables in a prop
vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

-- Generates all commbos of True and False for a given number
-- Like when you toss 2(n) coins there are 4 possible outcomes
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools(n - 1)

-- Generates all lookups table for a prop
substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
    where vs = rmdups (vars p)

-- remove duplicates
rmdups :: Eq a => [a] -> [a]
rmdups xs= foldl (\acc x -> if  x `elem` acc then acc else acc++[x]) [] xs



-- Abstract Machine
data Expr = Val Int | Add Expr Expr
data Op = EVAL Expr|  ADD Int
type Cont =  [Op]

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y : c) 

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c) m = exec c (n+m)

value :: Expr -> Int
value e = eval e []

