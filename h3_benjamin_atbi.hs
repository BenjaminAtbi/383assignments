
-- Q1
data List a = Empty | Cons a (List a) deriving (Eq, Ord, Show, Read)

listZip :: List a -> List b -> List (a,b)
listZip _ Empty        = Empty  
listZip Empty _        = Empty
listZip (Cons x xs) (Cons y ys) = Cons (x,y) (listZip xs ys)

-- Q2
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

insert :: Ord a => a -> Tree a -> Tree a
insert a EmptyTree = Node a EmptyTree EmptyTree
insert a (Node x left right)
    | a < x     = Node x (insert a left) right
    | a > x     = Node x left (insert a right)

-- Q3
data Nat a = Zero | Succ (Nat a) deriving (Eq, Show)

natPlus :: Nat a -> Nat a -> Nat a
natPlus Zero right = right
natPlus (Succ x) right = Succ (natPlus x right)

natMult :: Nat a -> Nat a -> Nat a
natMult Zero _ = Zero
natMult (Succ x) right = natPlus (natMult x right) right 

-- Q4
instance Eq a => Eq (Tree a) where 
    EmptyTree == EmptyTree          = True 
    EmptyTree == _                  = False
    _ == EmptyTree                  = False
    Node a al ar == Node b bl br    = a == b && al == bl && ar == br

-- Q5
data AssocList k v = ALEmpty | ALCons k v (AssocList k v) deriving (Show)

instance Functor (AssocList k) where
    fmap :: (v1 -> v2) -> AssocList k v1 -> AssocList k v2
    fmap f (ALCons k v) = AlCons k (f v)
    -- fmap f (ALCons k v) = AssocList k (f v)