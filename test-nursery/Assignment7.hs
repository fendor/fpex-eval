module Assignment7 where

class Monad m => Bunch m where
  -- Empty result (or no answer)
  zero :: m a
  -- All answers in xm or ym
  alt :: m a -> m a -> m a
  -- Answers yielded by ‘auxiliary calculations’
  -- (for now, think of wrap in terms of the
  -- identity, i.e., wrap = id)
  wrap :: m a -> m a

shuffleLP :: Bunch m => (Term, Term, Term) -> Pred m
shuffleLP (p,q,r) = undefined

(=:=) :: Bunch m => Term -> Term -> Pred m
(t=:=u)(MkAnswer(s,n)) =
    case unify(t, u) s of
      Just s' -> return(MkAnswer(s',n))
      Nothing -> zero

(&&&) :: Bunch m => Pred m -> Pred m -> Pred m
(p &&& q) s = p s >>= q

(|||) :: Bunch m => Pred m -> Pred m -> Pred m
(p ||| q) s = alt (p s) (q s)

infixr 4 =:=
infixr 3 &&&
infixr 2 |||

newtype Subst = MkSubst [(Variable, Term)] deriving Show
unSubst(MkSubst s) = s

idsubst = MkSubst[]
extend x t (MkSubst s) = MkSubst ((x,t):s)

apply :: Subst -> Term -> Term
apply s t =
        case deref s t of
          Cons x xs -> Cons (apply s x) (apply s xs)
          t'        -> t'

deref :: Subst -> Term -> Term
deref s (Var v) =
        case lookup v (unSubst s) of
          Just t    -> deref s t
          Nothing   -> Var v
deref s t = t

unify :: (Term, Term) -> Subst -> Maybe Subst
unify (t,u) s =
  case (deref s t, deref s u) of
    (Nil, Nil) -> Just s
    (Cons x xs, Cons y ys)  -> unify (x,y) s >>= unify (xs, ys)
    (Int n, Int m) | (n==m) -> Just s
    (Var x, Var y) | (x==y) -> Just s
    (Var x, t)              -> if occurs x t s then Nothing
                                      else Just (extend x t s)
    (t, Var x)              -> if occurs x t s then Nothing
                                      else Just (extend x t s)
    (_,_)                   -> Nothing

occurs :: Variable -> Term -> Subst -> Bool
occurs x t s =
  case deref s t of
    Var y     -> x == y
    Cons y ys -> occurs x y s || occurs x ys s
    _         -> False


append :: Bunch m => (Term, Term, Term) -> Pred m
append = undefined

proper :: Bunch m => Term -> Pred m
proper = undefined

data Term = Int Int
 | Nil
 | Cons Term Term
 | Var Variable deriving (Eq, Show)

step :: Bunch m => Pred m -> Pred m
step p s = wrap (p s)

exists :: Bunch m => (Term -> Pred m) -> Pred m
exists p (MkAnswer (s,n)) = p (Var $ Generated n) (MkAnswer (s,n+1))

type Pred m = Answer -> m Answer
data Variable = Named String | Generated Int deriving (Show, Eq)


newtype Answer = MkAnswer (Subst, Int) deriving Show






type Identifier = String

type Program_Name = Identifier

type PN = Program_Name

data P = P PN [S] deriving (Eq, Show) -- P for program

data S
  = Ass E E -- S for statement
  | If PE [S] [S]
  | While PE [S]
  | Repeat [S] PE
  deriving (Eq, Show)

data E
  = I Integer -- E for expression
  | F Float
  | V Variable
  | Plu E E
  | Min E E
  | Mul E E
  | Div E E
  deriving (Eq, Show)

data PE
  = Equal E E -- PE for predicate expression
  | NEqual E E
  | GEqual E E
  | LEqual E E
  deriving (Eq, Show)

type Parse1 a b = [a] -> [(b, [a])]

parser1 :: Parse1 Char P
parser1 = undefined

topLevel1 :: Parse1 a b -> [a] -> b
topLevel1 = undefined

newtype Parse2 a = Parse (String -> [(a, String)])

parser2 :: Parse2 P
parser2 = undefined

topLevel2 :: Parse2 a -> String -> a
topLevel2 = undefined