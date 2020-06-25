{-1.1.1
data List a = [] | a : List a

map f [] = []
map f (x:xs) = f x : map f xs

concat [] = []
concat (xs:xss) = xs ++ concat xss

axiom map_append: map f (xs ++ ys) .=. map f xs ++ map f ys

goal map f (concat xss) .=. concat (map (map f) xss)

---
-}

{-1.1.2
data Nat = Z | Suc Nat

add Z m = m
add (Suc n) m = Suc (add n m)

goal add (add x y) z .=. add x (add y z)

---
-}

{-1.2.1
data AExp = Val Integer | Add AExp AExp | Mul AExp AExp

eval (Val i) = i
eval (Add a b) = (eval a) + (eval b)
eval (Mul a b) = (eval a) * (eval b)

simp (Val i) = Val i
simp (Mul a b) = Mul (simp a) (simp b)
simp (Add a b) = if a == Val 0 then simp b else Add (simp a) (simp b)

axiom addZero: x + 0 .=. x
axiom zeroAdd: 0 + x .=. x

goal eval (simp e) .=. eval e

---
-}

{-1.3.1
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

itrev [] xs = xs
itrev (x:xs) ys = itrev xs (x:ys)

axiom app_assoc: (xs ++ ys) ++ zs .=. xs ++ (ys ++ zs)

goal itrev xs [] .=. reverse xs

---
-}

{-1.4.1
data List a = [] | a : List a

foldl g b [] = b
foldl g b (x:xs) = foldl g (g b x) xs

foldr g b [] = b
foldr g b (x:xs) = g x (foldr g b xs)

axiom f_assoc: f x (f y z) .=. f (f x y) z
axiom f_comm_a: f x a .=. f a x

goal foldl f a .=. foldr f a

---
-}

{-1.5.1
data List a = [] | a : List a

sum [] = 0
sum (x:xs) = x + sum xs

sum2 [] [] = 0
sum2 [] (y:ys) = y + sum2 ys []
sum2 (x:xs) ys = x + sum2 xs ys

axiom zeroAdd: 0 + b .=. b
axiom addZero: b + 0 .=. b
axiom addComm: a + b .=. b + a
axiom addAssoc: a + (b + c) .=. (a + b) + c

goal sum2 xs ys .=. sum xs + sum ys

---
-}


{-2.1.1
data List a = [] | a : List a

sum xs = sum_aux xs 0

sum_aux [] acc = acc
sum_aux (x:xs) acc = sum_aux xs (acc+x)

[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

axiom zeroAdd: 0 + b .=. b
axiom addZero: b + 0 .=. b
axiom addComm: a + b .=. b + a
axiom addAssoc: a + (b + c) .=. (a + b) + c

goal sum (xs ++ ys) .=. sum xs + sum ys

---
-}

{-2.1.2a
data List a = [] | a : List a

reverse [] = []
reverse (x : xs) = snoc (reverse xs) x

snoc [] y = [y]
snoc (x : xs) y = x : snoc xs y

goal reverse (snoc xs x) .=. x : reverse xs

---
-}

{-2.1.2b
data List a = [] | a : List a

reverse [] = []
reverse (x : xs) = snoc (reverse xs) x

snoc [] y = [y]
snoc (x : xs) y = x : snoc xs y

goal reverse (reverse xs) .=. xs

---
-}

{-2.1.3
data List a = [] | a : List a
data Tree a = Leaf | Node (Tree a) a (Tree a)

sumTree Leaf = 0
sumTree (Node l x r) = sumTree l + x + sumTree r

sum [] = 0
sum (x:xs) = x + sum xs

inorder Leaf = []
inorder (Node l x r) = (inorder l) ++ (x : (inorder r))

[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

axiom zeroAdd: 0 + b .=. b
axiom addAssoc: a + (b + c) .=. (a + b) + c

goal sum (inorder t) .=. sumTree t

---
-}

{-2.2.1
data Tree a = L | N (Tree a) a (Tree a)

flat L = []
flat (N l x r) = flat l ++ (x : flat r)

app L xs = xs
app (N l x r) xs = app l (x : app r xs)

[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

axiom app_assoc: (xs ++ ys) ++ zs .=. xs ++ (ys ++ zs)
axiom app_nil: xs ++ [] .=. xs
axiom nil_app: [] ++ xs .=. xs

goal app t [] .=. flat t

---
-}

{-2.3.1
data List a = [] | a : List a
data Bool = True | False

filter f [] = []
filter f (x : xs) = if f x then x : filter f xs else filter f xs

(f . g) x = f (g x)

axiom if_True: (if True then x else y) .=. x
axiom if_False: (if False then x else y) .=. y

goal filter p . filter p .=. filter p

---
-}

{-2.4.1
data List a = [] | a : List a

drop2 [] = []
drop2 [x] = [x]
drop2 (x:y:xs) = x : drop2 xs

length [] = 0
length (x:xs) = 1 + length xs

axiom: addZeroOne: 0 + 1 .=. 1
axiom: addOneZero: 1 + 0 .=. 1
axiom: addOneOne: 1 + 1 .=. 2
axiom: addAssoc: (a + b) + c .=. b + (b + c)
axiom: divOneTwo: 1 `div` 2 .=. 0
axiom: divTwoTwo: 2 `div` 2 .=. 1
axiom: divMulOneTwo: 1 + (x `div` 2) .=. (x + 2) `div` 2

goal length (drop2 xs) .=. (length xs + 1) `div 2

---
-}

{-2.4.2
data List a = [] | a : List a
data Bool = True | False

length [] = 0
length (x:xs) = 1 + length xs

countGt [] ys = 0
countGt (x:xs) [] = length (x:xs)
countGt (x:xs) (y:ys) = if x > y then 1 + countGt (x:xs) ys else countGt (y:ys) xs

axiom zeroAdd: 0 + b .=. b
axiom addZero: b + 0 .=. b
axiom addComm: a + b .=. b + a
axiom addAssoc: a + (b + c) .=. (a + b) + c
axiom ifTrue: (if True then a else b) .=. a
axiom ifFalse: (if False then a else b) .=. b
axiom lengthNonneg: 0 <= length xs
axiom zeroLeOne: 0 <= 1
axiom leAddMono: y <= z ==> x + y <= x + z

goal countGt xs ys <= length xs + length ys

---
-}
