module ProblemSet_2 where

import Data.List ((\\),intercalate,intersect,nub,sort)
import qualified Data.Set as Set
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

{-1.4.2b
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
data Relation a = Relation (Set.Set (a,a))

{-2.2.3-}
foldGraph :: Ord a => (a -> b -> b -> [a] -> [a] -> b) -> b -> Relation a -> a -> b
foldGraph f z (Relation edges) u = aux [u] []
  where
    aux [] explored = z
    aux (u:frontier) explored =
      let newExplored = explored ++ [u]
          neighbors = sort $ map snd $ filter ((==u) . fst) $ Set.toList edges
          l = aux (neighbors \\ newExplored) newExplored
          r = aux frontier newExplored
      in f u l r explored neighbors

{-2.2.3a-}
dfs :: Ord a => Relation a -> a -> [a]
dfs = foldGraph (\u xs ys _ _ -> [u] ++ (nub (xs ++ ys))) []

{-2.2.3b-}
isAcyclic :: Eq a => [a] -> [a] -> Bool
isAcyclic explored neighbors = null (intersect explored neighbors)

dag :: Ord a => Relation a -> Bool
dag r@(Relation edges)
  | Set.null edges = True
  | otherwise      =
    let u = fst $ head $ Set.toList edges
    in foldGraph (\_ b1 b2 explored neighbors -> b1 && b2 && isAcyclic explored neighbors) True r u

-- {-2.2.3c-}
topology :: Ord a => Relation a -> a -> Maybe [a]
topology r u = foldGraph aux (Just []) r u
  where
    aux u (Just xs) (Just ys) explored neighbors =
      let acyclic = isAcyclic explored neighbors
      in if acyclic then Just (xs ++ [u] ++ ys) else Nothing
    aux _ _ _ _ _ = Nothing

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
data Type = TypeVar Char | Type String [Type]
  deriving (Eq)

{-2.2.5a-}
instance Show Type where
  show (TypeVar x) = [x]
  show (Type x []) = x
  show (Type x xs)
    | x == "Tuple" = "(" ++ intercalate ", " args ++ ")"
    | x == "List"  = "[" ++ intercalate ", " args ++ "]"
    | otherwise    = "(" ++ x ++ " " ++ intercalate " " args ++ ")"
      where
        args = map show xs

{-2.2.5b-}
unpackEither :: [Either a b] -> Either [a] [b]
unpackEither xs = aux xs [] []
  where
    aux [] [] zs = Right zs
    aux [] ys _ = Left ys
    aux ((Left x):xs) ys zs = aux xs (ys ++ [x]) zs
    aux ((Right x):xs) ys zs = aux xs ys (zs ++ [x])

unify :: Type -> Type -> Either String Type
unify (TypeVar _) t = Right t
unify t (TypeVar _) = Right t
unify t@(Type x xs) u@(Type y ys)
  | x == y && length xs == length ys =
    let args = unpackEither $ map (uncurry unify) $ zip xs ys
    in case args of
      Left  zs -> Left $ intercalate ", " zs
      Right zs -> Right $ Type x zs
  | otherwise = Left $ "Couldn't match type `" ++ show t ++ "` with `" ++ show u ++ "`"

{-2.3.1 this task should be done in separate files called AssocList.hs and AssocListTests.hs-}

{-2.4.1a
  filter not
  Step 1
  no variables
  Step 2
  - not :: Bool -> Bool
  - filter :: (a -> Bool) -> [a] -> [a]
  Step 3
  - from "filter not" derive "a -> Bool = Bool -> Bool"
  Step 4
  - from "a -> Bool = Bool -> Bool":
    - a = Bool
  - no simplifications possible. Return:
    filter not :: [Bool] -> [Bool]
-}

{-2.4.1b
  [] : []
  Step 1
  no variables
  Step 2
  - [] :: [a]
  - (:) :: b -> [b] -> [b]
  - [] :: [c]
  Step 3
  - from "[] : []" derive "b -> [b] = [a] -> [c]"
  Step 4
  - from "b -> [b] = [a] -> [c]":
    - b = [a]
    - [b] = [c]
  - no simplifications possible. Return:
    [] : [] :: [[a]]
-}

{-2.4.1c
  \f x y -> f (x,y)
  Step 1
  - f :: a, x :: b, y :: c
  Step 2
  no functions
  Step 3
  - from "f (x,y)" derive "a = (b,c) -> d"
  Step 4
  - no simplifications possible. Return:
    \f x y -> f (x,y) :: ((b,c) -> d) -> b -> c -> d
-}

{-2.4.1d
  map (map fst)
  Step 1
  no variables
  Step 2
  - map :: (a -> b) -> [a] -> [b]
  - map :: (c -> d) -> [c] -> [d]
  - fst :: (e,f) -> e
  Step 3
  - from "map fst" derive "c -> d = (e,f) -> e"
  - from "map (map fst)" derive "a -> b = [c] -> [d]"
  Step 4
  - from "c -> d = (e,f) -> e":
    - c = (e,f)
    - d = e
  - from "a -> b = [c] -> [d]" and "c = (e,f)" and "d = e":
    - a = [(e,f)]
    - b = [e]
  - no further simplifications possible. Return:
    map (map fst) :: [[(e,f)]] -> [[e]]
-}

{-2.4.2
  The function `map head` has the type `[[a]] -> [a]`. Therefore, it expects
  a list of lists as first argument. In this case the first argument is a list
  of `Bool`s.
-}

{-2.4.3a
  curry id
  Step 1
  no variables
  Step 2
  - curry :: ((a,b) -> c) -> (a -> b -> c)
  - id :: d -> d
  Step 3
  - from "curry id" derive "(a,b) -> c = d -> d"
  Step 4
  - from "(a,b) -> c = d -> d":
    - d = (a,b)
    - d = c
  - from "d = (a,b)" and "d = c":
    - c = (a,b)
  - no further simplifications possible. Return:
    curry id :: a -> b -> (a,b)
-}

{-2.4.3b
  uncurry id
  Step 1
  no variables
  Step 2
  - uncurry :: (a -> b -> c) -> ((a,b) -> c)
  - id :: d -> d
  Step 3
  - from "uncurry id" derive "a -> b -> c = d -> d"
  Step 4
  - from "a -> b -> c = d -> d":
    - d = a
    - d = b -> c
  - from "d = a" and "d = b -> c":
    - a = b -> c
  - no further simplifications possible. Return:
    uncurry id :: (b -> c, b) -> c
-}

{-2.4.3c
  curry (curry id)
  Step 1
  no variables
  Step 2
  - curry :: ((a,b) -> c) -> (a -> b -> c)
  - curry :: ((d,e) -> f) -> (d -> e -> f)
  - id :: g -> g
  Step 3
  - from "curry id" derive "(d,e) -> f = g -> g"
  - from "curry (curry id)" derive "(a,b) -> c = d -> e -> f"
  Step 4
  - from "(d,e) -> f = g -> g":
    - g = (d,e)
    - g = f
  - from "g = (d,e)" and "g = f":
    - f = (d,e)
  - from "(a,b) -> c = d -> e -> f" and "f = (d,e)":
    - d = (a,b)
    - c = e -> (d,e)
  - no further simplifications possible. Return:
    curry (curry id) :: a -> b -> e -> ((a,b),e)
-}

{-2.4.3d
  uncurry (uncurry id)
  Step 1
  no variables
  Step 2
  - uncurry :: (a -> b -> c) -> ((a,b) -> c)
  - uncurry :: (d -> e -> f) -> ((d,e) -> f)
  - id :: g -> g
  Step 3
  - from "uncurry id" derive "d -> e -> f = g -> g"
  - from "uncurry (uncurry id)" derive "a -> b -> c = (d,e) -> f"
  Step 4
  - from "d -> e -> f = g -> g":
    - g = d
    - g = e -> f
  - from "g = d" and "g = e -> f":
    - d = e -> f
  - from "a -> b -> c = (d,e) -> f":
    - a = (d,e)
    - f = b -> c
  - from "a = (d,e)" and "d = e -> f" and " f = b -> c":
    - a = (e -> b -> c, e)
  - no further simplifications possible. Return:
    uncurry (uncurry id) :: ((e -> b -> c, e), b) -> c
-}

{-2.4.3e
  uncurry (curry id)
  Step 1
  no variables
  Step 2
  - uncurry :: (a -> b -> c) -> ((a,b) -> c)
  - curry :: ((d,e) -> f) -> (d -> e -> f)
  - id :: g -> g
  Step 3
  - from "curry id" derive "(d,e) -> f = g -> g"
  - from "uncurry (curry id)" derive "a -> b -> c = d -> e -> f"
  Step 4
  - from "(d,e) -> f = g -> g":
    - g = (d,e)
    - g = f
  - from "g = (d,e)" and "g = f":
    - f = (d,e)
  - from "a -> b -> c = d -> e -> f":
    - a = d
    - b = e
    - c = f
  - from "c = f" and "f = (d,e)":
    - c = (d,e)
  - from "c = (d,e)" and "d = a" and "e = b":
    - c = (a,b)
  - no further simplifications possible. Return:
    uncurry (curry id) :: (a,b) -> (a,b)
-}

{-2.4.4a
  The first argument of `uncurry` is a function taking two curried arguments.
  The first argument of the function `curry` expects as first argument is a
  tuple. The types `(a,b)` and `c -> d -> e` cannot be unified.
-}

{-2.4.4b
  The first argument of `curry` is a function taking a tuple as first argument.
  The innermost `curry` returns a function taking two curried arguments.
  The types `(a,b) -> c` and `d -> e -> f` cannot be unified.
-}

{-2.4.5a
  foldr (\x y -> y ++ x) []
  Step 1
  no variables
  Step 2
  - foldr :: (a -> b -> b) -> b -> [a] -> b
  - (\x y -> y ++ x) :: c -> d -> e
  - (++) :: [f] -> [f] -> [f]
  - [] :: [g]
  Step 3
  - from "y ++ x" derive "[f] = c", "[f] = d" and "[f] = e"
  - from "foldr (\x y -> y ++ x) []" derive "(a -> b -> b) -> b = (c -> d -> e) -> [g]"
  Step 4
  - from "(a -> b -> b) -> b = (c -> d -> e) -> [g]":
    - a = c
    - b = d
    - b = e
    - b = [g]
  - from "c = [f]", "d = [f]" and "e = [f]":
    - a = [f]
    - b = [f]
  - from "b = [f]" and "b = [g]":
    - [f] = [g], f = g
  - no further simplifications possible. Return:
    foldr (\x y -> y ++ x) [] :: [[f]] -> [f]
-}

{-2.4.5b
  (\f g x -> g $ f $ x)
  Step 1
  - f :: a, g :: b, x :: c
  Step 2
  no functions
  Step 3
  - from "f $ x" derive "a = c -> d"
  - from "g $ f $ x" derive "b = d -> e"
  Step 4
  - no further simplifications possible. Return:
    (\f g x -> g $ f $ x) :: (c -> d) -> (d -> e) -> c -> e
-}

{-2.4.5c
  (:[1,2])
  Step 1
  no variables
  Step 2
  - (:) :: a -> [a] -> [a]
  - [1,2] :: Num b -> [b]
  Step 3
  - from "(:[1,2])" derive "[a] = [b]"
  Step 4
  - from "[a] = [b]"
    - a = b
  - no further simplifications possible. Return:
    (:[1,2]) :: Num b => b -> [b]
-}

{-2.4.5d
  map head . map (\f -> f "hello")
  Step 1
  no variables
  Step 2
  - map :: (a -> b) -> [a] -> [b]
  - head :: [c] -> c
  - (.) :: (e -> f) -> (d -> e) -> (d -> f)
  - map :: (g -> h) -> [g] -> [h]
  - (\f -> f "hello") :: i -> j
  Step 3
  - from "f "hello"" derive "i = String -> j"
  - from "map (\f -> f "hello")" derive "g -> h = i -> j"
  - from "map head" derive "a -> b = [c] -> c"
  - from "map head . map (\f -> f "hello")" derive "e -> f = [a] -> [b]" and "d -> e = [g] -> [h]"
  Step 4
  - from "g -> h = i -> j":
    - g = i
    - h = j
  - from "g = i" and "i = String -> j":
    - g = String -> j
  - from "a -> b = [c] -> c":
    - a = [c]
    - b = c
  - from "e -> f = [a] -> [b]":
    - e = [a]
    - f = [b]
  - from "e = [a]" and "a = [c]":
    - e = [[c]]
  - from "f = [b]" and "b = c":
    - f = [c]
  - from "d -> e = [g] -> [h]":
    - d = [g]
    - e = [h]
  - from "d = [g]" and "g = String -> j":
    - d = [String -> j]
  - from "e = [h]" and "h = j":
    - e = [j]
  - from "e = [[c]]" and "e = [j]":
    - [j] = [[c]], j = [c]
  - from "j = [c]" and "d = [String -> j]":
    - d = [String -> [c]]
  - no further simplifications possible. Return:
    map head . map (\f -> f "hello") :: [String -> [c]] -> [c]
-}
