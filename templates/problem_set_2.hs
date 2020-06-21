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
  size = undefined
  member = undefined
  extractMin = undefined
  update = undefined
  partition = undefined

{-1.2.1a-}
collatz :: Integer -> [Integer]
collatz = undefined

{-1.2.1b-}
unfold :: (a -> Maybe a) -> a -> [a]
unfold = undefined

{-1.2.1c-}
next :: a
next = undefined

{-1.2.2a-}
data Tree a = TODO1

{-1.2.2b-}
sumTree :: Num a => Tree a -> a
sumTree = undefined

{-1.2.2c-}
cut :: Tree a -> Integer -> Tree a
cut = undefined

{-1.2.2d-}
foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree = undefined

{-1.2.2e-}
inorder :: Tree a -> [a]
inorder = undefined

{-1.2.2f-}
findAll :: (a -> Bool) -> Tree a -> [a]
findAll = undefined

{-1.2.3-}
type Name = String
data Atom = F | T | V Name deriving (Eq, Show)
data Conj = A Atom | Conj :&: Conj deriving (Eq, Show)

{-1.2.3a-}
contains :: Conj -> Atom -> Bool
contains = undefined

{-1.2.3b-}
implConj :: Conj -> Conj -> Bool
implConj = undefined

{-1.2.4-}
data Term = TODO2

{-1.2.4a-}
instance Show Term where
  show = undefined

{-1.2.4b-}
freeVars :: Term -> [String]
freeVars = undefined

{-1.2.4c-}
substVar :: String -> Term -> Term -> Term
substVar = undefined

{-1.2.5-}
g :: (a -> a') -> (b -> b') -> Either a b -> Either a' b'
g = undefined

h :: (a -> Either a b) -> a -> b
h = undefined

{-1.3.1 this task should be done in a separate file called Vector.hs-}

{-1.4.1
-}

{-1.4.2a
-}

{-1.4.2b
-}

{-1.4.3a
-}

{-1.4.3b
-}


{-2.1.1-}
data Pair a = Pair a a deriving (Show)

{-2.2.1a-}
safeHead :: [a] -> Maybe a
safeHead = undefined

safeLast :: [a] -> Maybe a
safeLast = undefined

{-2.2.1b-}
safeHead' :: [a] -> Maybe a
safeHead' = undefined

safeLast' :: [a] -> Maybe a
safeLast' = undefined

{-2.2.1c-}
select :: Eq a => a -> [(a,b)] -> [b]
select = undefined

{-2.2.2-}
data Node = File String | Dir String [Node]
type FileSys = [Node]

removeFiles :: String -> FileSys -> FileSys
removeFiles = undefined

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
dfs = undefined

{-2.2.3b-}
isAcyclic :: Eq a => [a] -> [a] -> Bool
isAcyclic  = undefined

dag :: Ord a => Relation a -> Bool
dag = undefined

{-2.2.3c-}
topology :: Ord a => Relation a -> a -> Maybe [a]
topology = undefined

{-2.2.4a-}
data Atom' = TODO3
data Literal = TODO4
data Formula = TODO5

{-2.2.4b-}
top :: Literal
top = undefined

bottom :: Literal
bottom = undefined

{-2.2.4c-}
type Clause = [Literal]
type CNF = [Clause]

conjToForm :: CNF -> Formula
conjToForm = undefined

{-2.2.4d-}
type Valuation = [(Name,Bool)]

substConj :: Valuation -> CNF -> CNF
substConj = undefined

{-2.2.4e-}
simpConj :: CNF -> CNF
simpConj = undefined

{-2.2.4f-}
cnf :: Formula -> CNF
cnf = undefined

{-2.2.5-}
data Type = TypeVar Char | Type String [Type]
  deriving (Eq)

{-2.2.5a-}
instance Show Type where
  show = undefined

{-2.2.5b-}
unify :: Type -> Type -> Either String Type
unify = undefined

{-2.3.1 this task should be done in separate files called AssocList.hs and AssocListTests.hs-}

{-2.4.1a
-}

{-2.4.1b
-}

{-2.4.1c
-}

{-2.4.1d
-}

{-2.4.2
-}

{-2.4.3a
-}

{-2.4.3b
-}

{-2.4.3c
-}

{-2.4.3d
-}

{-2.4.3e
-}

{-2.4.4a
-}

{-2.4.4b
-}

{-2.4.5a
-}

{-2.4.5b
-}

{-2.4.5c
-}

{-2.4.5d
-}
