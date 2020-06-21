module ProblemSet_2 where

import Data.List (nub)
import Data.Set (Set)
import Data.Semigroup


{-1.1.1-}
class IntContainer c where
  -- the empty container
  empty :: c
  -- insert an integer into a container
  insert :: Integer -> c -> c

class IntContainer c => IntCollection c where
  -- the number of integers in the collection
  size :: c -> Integer
  -- True if and only if the integer is a member of the collection
  member :: Integer -> c -> Bool
  -- extracts the smallest number in the collection
  -- if such a number exists.
  extractMin :: c -> Maybe Integer
  -- "update f c" applies f to every element e of c.
  -- If "f c" returns Nothing, the element is deleted;
  -- otherwise, the new value is stored in place of e.
  update :: (Integer -> Maybe Integer) -> c -> c
  -- "partition p c" creates two collections (c1 , c2) such that
  -- c1 contains exactly those elements of c satisfying p and
  -- c2 contains exactly those elements of c not satisfying p.
  partition :: (Integer -> Bool) -> c -> (c,c)

-- datatype C is only given to make this program compile, the instantiation of
-- C as IntCollection should be independent of the implementation of C!
data C = C [Integer]
instance IntContainer C where
  empty = C []
  insert x (C xs) = C (x:xs)
fold :: (Integer -> b -> b) -> b -> C -> b
fold f acc (C xs) = foldr f acc xs

instance IntCollection C where
  size = fold (const (+1)) 0
  member x = fold ((||) . (==x)) False
  extractMin = fold aux Nothing
    where
      aux x Nothing = Just x
      aux x (Just y) = Just (min x y)
  update f = fold (aux . f) empty
    where
      aux Nothing = id
      aux (Just x) = insert x
  partition p = fold aux (empty,empty)
    where
      aux x (c1,c2)
        | p x       = (insert x c1, c2)
        | otherwise = (c1, insert x c2)

{-1.2.1a-}
collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n
  | even n    = n : collatz (n `div` 2)
  | otherwise = n : collatz (n*3 + 1)

{-1.2.1b-}
unfold :: (a -> Maybe a) -> a -> [a]
unfold f a = a : rest (f a)
  where
    rest Nothing = []
    rest (Just x) = unfold f x

{-1.2.1c-}
next :: Integer -> Maybe Integer
next 1 = Nothing
next n
  | even n    = Just (n `div` 2)
  | otherwise = Just (n*3 + 1)

{-1.2.2a-}
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

{-1.2.2b-}
sumTree :: Num a => Tree a -> a
sumTree Leaf = 0
sumTree (Node l v r) = sumTree l + v + sumTree r

{-1.2.2c-}
cut :: Tree a -> Integer -> Tree a
cut Leaf _ = Leaf
cut (Node l v r) n
  | n <= 0 = Leaf
  | otherwise = Node (cut l (n - 1)) v (cut r (n - 1))

{-1.2.2d-}
foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree f init Leaf = init
foldTree f init (Node l a r) =
  let right = foldTree f init r
      mid   = f a right
      left  = foldTree f mid l
  in left

{-1.2.2e-}
inorder :: Tree a -> [a]
inorder = foldTree (:) []

{-1.2.2f-}
findAll :: (a -> Bool) -> Tree a -> [a]
findAll p = foldTree testP []
  where
    testP x acc = if p x then x:acc else acc

{-1.2.3-}
type Name = String
data Atom = F | T | V Name deriving (Eq, Show)
data Conj = A Atom | Conj :&: Conj deriving (Eq, Show)

{-1.2.3a-}
contains :: Conj -> Atom -> Bool
contains (A a) a' = a == a'
contains (c1 :&: c2) a = contains c1 a || contains c2 a

{-1.2.3b-}
implConj :: Conj -> Conj -> Bool
implConj c (A a) = a == T || contains c F || contains c a
implConj c (c1 :&: c2) = implConj c c1 && implConj c c2

{-1.2.4-}
data Term = App Term Term | Abs String Term | Var String

{-1.2.4a-}
instance Show Term where
  show (Var x) = x
  show (Abs x t) = "(\\" ++ x ++ " -> " ++ show t ++ ")"
  show (App t1 t2@(App _ _)) = show t1 ++ " (" ++ show t2 ++ ")"
  show (App t1 t2) = show t1 ++ " " ++ show t2

{-1.2.4b-}
freeVars :: Term -> [String]
freeVars = freeVarsB []
  where
    freeVarsB bs (Var x)
      | x `elem` bs = []
      | otherwise   = [x]
    freeVarsB bs (App t1 t2) = nub $ freeVarsB bs t1 ++ freeVarsB bs t2
    freeVarsB bs (Abs x t) = freeVarsB (x:bs) t

{-1.2.4c-}
substVar :: String -> Term -> Term -> Term
substVar v term (Var x)
  | v == x    = term
  | otherwise = Var x
substVar v term (Abs x t)
  | v == x    = Abs x t
  | otherwise = Abs x (substVar v term t)
substVar v term (App t1 t2) = App (substVar v term t1) (substVar v term t2)

{-1.2.5-}
g :: (a -> a') -> (b -> b') -> Either a b -> Either a' b'
g f _ (Left  x) = Left  (f x)
g _ f (Right x) = Right (f x)

h :: (a -> Either a b) -> a -> b
-- Note that this does not necessarily terminate, because f may never produce a Left value.
-- In general it is not possible to write a version of h that terminates on arbitrary f.
h f x = case f x of
          Left  x' -> h f x'
          Right y  -> y

{-1.3.1 this task should be done in a separate file called Vector.hs-}

{-1.4.1
  `xs == []` is only type correct if the type of elements of the list are an
  instance if `Eq`. Therefore, the function `\xs -> xs == []` has the type
  `Eq a -> [a] -> Bool` which is less general than the type of `null`.
-}

{-1.4.2a
  f u v = min (head u) (last (concat v))
  Step 1
  - u :: a, v :: b
  Step 2
  - head :: [c] -> c
  - concat :: [[d]] -> [d]
  - last :: [e] -> e
  - min :: Ord f => f -> f -> f
  Step 3
  - from "head u" derive "a = [c]"
  - from "concat v" derive "b = [[d]]"
  - from "last (concat v)" derive "[d] = [e]"
  - from "min (head u) (last (concat v))" derive "c = f" and "e = f"
  Step 4
  - apply "a = [c]" and update:
    - u :: [c]
  - apply "b = [[d]]" and update:
    - v :: [[d]]
  - apply "[d] = [e]" to get "d = e" and update:
    - v :: [[e]]
    - concat :: [[e]] -> [e]
  - apply "c = f" and update:
    - u :: [f]
    - head :: [f] -> f
  - apply "e = f" and update:
    - v :: [[f]]
    - concat :: [[f]] -> [f]
    - last :: [[f]] -> [f]
  - no simplifications possible. Return:
    f :: Ord f => [f] -> [[f]] -> f
-}

{-1.4.2b
  ffoldl = foldl . foldl
  Step 1
  no variables
  Step 2
  - foldl :: (a -> b -> a) -> a -> [b] -> a
  - (.) :: (d -> e) -> (c -> d) -> c -> e
  - foldl :: (f -> g -> f) -> f -> [g] -> f
  Step 3
  - from "(.) foldl" derive "d -> e = (a -> b -> a) -> a -> [b] -> a"
  - from "(.) foldl foldl" derive "c -> d = (f -> g -> f) -> f -> [g] -> f"
  - also note ffoldl :: c -> e
  Step 4
  - from "d -> e = (a -> b -> a) -> a -> [b] -> a":
    - d = (a -> b -> a)
    - e = a -> [b] -> a
  - from "c -> d = (f -> g -> f) -> f -> [g] -> f":
    - c = (f -> g -> f)
    - d = f -> [g] -> f
  - from "d = (a -> b -> a)" and "d = f -> [g] -> f":
    - a = f
    - b = [g]
  - apply "a = f" and "b = [g]" and update:
    - d = f -> [g] -> f
    - e = f -> [[g]] -> f
  - no more simplification possible.
    ffoldl :: (f -> g -> f) -> f -> [[g]] -> f
-}

{-1.4.2c
  f x y = y : map (++x) y
  Step 1
  x :: a, y :: b
  Step 2
  - (:) :: c -> [c] -> [c]
  - map :: (d -> e) -> [d] -> [e]
  - (++) :: [f] -> [f] -> [f]
  Step 3
  - from "++x" derive a = [f]
  - from "map (++x) y" derive d -> e = [f] -> [f] and [d] = b
  - from "y : map (++x) y" derive c = b and [c] = [e]
  Step 4
  - from "d -> e = [f] -> [f]"
    - d = [f]
    - e = [f]
  - from "c = b" and "[d] = b"
    - c = [d]
  - from "d = [f]" and "c = [d]"
    - c = [[f]]
  - from "c = [[f]]" and "[c] = [e]"
    - e = [[f]]
  - contradiction "e = [[f]]" and "e = [f]"
    implies "f = [f]"

  --> Type Error
-}

{-1.4.3a
  `then` branch returns `Int`, `else` branch `String`
-}

{-1.4.3b
  `f x :: String`, there is no instance of `Num` for `Char`
-}


{-2.1.1-}
data Pair a = Pair a a deriving (Show)

instance Semigroup a => Semigroup (Pair a) where
  Pair a c <> Pair b d = Pair (a <> b) (c <> d)

instance Monoid a => Monoid (Pair a) where
  mempty = Pair mempty mempty

{-2.2.1a-}
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_ : xs) = safeLast xs

{-2.2.1b-}
safeHead' :: [a] -> Maybe a
safeHead' = foldr (\h _ -> Just h) Nothing

safeLast' :: [a] -> Maybe a
safeLast' = foldr aux Nothing
  where
    aux l Nothing = Just l
    aux _ l = l

{-2.2.1c-}
select :: Eq a => a -> [(a,b)] -> [b]
select x = map snd . filter (\(a,b) -> x == a)

{-2.2.2-}
data Node = File String | Dir String [Node]
type FileSys = [Node]

removeFiles :: String -> FileSys -> FileSys
removeFiles _ [] = []
removeFiles name (File name' : fs) =
  (if name' == name then [] else [File name']) ++ removeFiles name fs
removeFiles name (Dir name' fs' : fs) =
  Dir name' (removeFiles name fs') : removeFiles name fs

{-2.2.3-}
data Relation a = Relation (Set (a,a))

{-2.2.3a-}
dfs :: Relation a -> a -> [a]
dfs = undefined

{-2.2.3b-}
dag :: Relation a -> Bool
dag = undefined

{-2.2.3c-}
topology :: Relation a -> Maybe [a]
topology = undefined

{-2.2.3d-}
connect :: Relation a -> Set (Relation a)
connect = undefined

{-2.2.3e-}
classes :: Relation a -> Set (Set a)
classes = undefined

{-2.2.4a-}
data Atom' = T' | V' Name
  deriving (Eq, Show)
data Literal = Pos Atom' | Neg Atom'
  deriving (Eq, Show)
data Formula = L Literal | Formula :&&: Formula | Formula :||: Formula
  deriving (Eq, Show)

{-2.2.4b-}
top :: Literal
top = Pos T'

bottom :: Literal
bottom = Neg T'

{-2.2.4c-}
type Clause = [Literal]
type CNF = [Clause]

clauseToForm :: Clause -> Formula
clauseToForm [] = L bottom
clauseToForm ls = foldr ((:||:) . L) (L $ last ls) (init ls)

conjToForm :: CNF -> Formula
conjToForm [] = L top
conjToForm ds = foldr ((:&&:) . clauseToForm) (clauseToForm $ last ds) (init ds)

{-2.2.4d-}
type Valuation = [(Name,Bool)]

substLiteral :: Valuation -> Literal -> Literal
substLiteral v l@(Pos (V' n)) = case lookup n v of
  Just b  -> if b then top else bottom
  Nothing -> l
substLiteral v l@(Neg (V' n)) = case lookup n v of
  Just b  -> if b then bottom else top
  Nothing -> l
substLiteral v l = l

substClause :: Valuation -> Clause -> Clause
substClause = map . substLiteral

substConj :: Valuation -> CNF -> CNF
substConj = map . substClause

{-2.2.4e-}
simpClause :: Clause -> Clause
simpClause = foldr collect []
  where
    collect _ [l] | l == top    = [top]
    collect l _   | l == top    = [top]
    collect l acc | l == bottom = acc
    collect a acc               = a:acc

simpConj :: CNF -> CNF
simpConj = foldr collect [] . map simpClause
  where
    collect _   [[]]           = [[]]
    collect []  _              = [[]]
    collect [l] acc | l == top = acc
    collect a   acc            = a:acc

{-2.2.4f-}
cnf :: Formula -> CNF
cnf (L l) = [[l]]
cnf (f1 :&&: f2) = cnf f1 ++ cnf f2
cnf (f1 :||: f2) = [c1 ++ c2 | c1 <- cnf f1, c2 <- cnf f2]

{-2.2.5-}
data Type = TypeVar Char | Type String [Type] deriving Eq

{-2.2.5a-}
instance Show Type where
  show = undefined

{-2.2.5b-}
unify :: Type -> Type -> Either String Type
unify = undefined

{-2.3.1 this task should be done in separate files called AssocList.hs and AssocListTests.hs-}

{-2.4.1
-}

{-2.4.2
-}

{-2.4.3
-}

{-2.4.4
-}

{-2.4.5
-}
