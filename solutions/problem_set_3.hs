{-1.1.1
data List a = [] | a : List a

map f [] = []
map f (x:xs) = f x : map f xs

concat [] = []
concat (xs:xss) = xs ++ concat xss

axiom map_append: map f (xs ++ ys) .=. map f xs ++ map f ys

goal map f (concat xss) .=. concat (map (map f) xss)

---

Lemma: map f (concat xss) .=. concat (map (map f) xss)
Proof by induction on List xss

Case []
  To show: map f (concat []) .=. concat (map (map f) [])
  Proof
                          map f (concat [])
      (by def concat) .=. map f []
      (by def map)    .=. []

                          concat (map (map f) [])
      (by def map)    .=. concat []
      (by def concat) .=. []
  QED

Case (xs:xss)
  To show: map f (concat (xs:xss)) .=. concat (map (map f) (xs:xss))
  IH:      map f (concat xss)      .=. concat (map (map f) xss)

  Proof
                          map f (concat (xs:xss))
      (by def concat) .=. map f (xs ++ concat xss)
      (by map_append) .=. map f xs ++ map f (concat xss)
      (by IH)         .=. map f xs ++ concat (map (map f) xss)

                          concat (map (map f) (xs:xss))
      (by def map)    .=. concat (map f xs : map (map f) xss)
      (by def concat) .=. map f xs ++ concat (map (map f) xss)
  QED
QED
-}

{-1.1.2
data Nat = Z | Suc Nat

add Z m = m
add (Suc n) m = Suc (add n m)

goal add (add x y) z .=. add x (add y z)

---

Lemma: add (add x y) z .=. add x (add y z)
Proof by induction on Nat x

Case Z
  To show: add (add Z y) z .=. add Z (add y z)
  Proof
                       add (add Z y) z
      (by def add) .=. add y z
      (by def add) .=. add Z (add y z)
  QED

Case (Suc x)
  To show: add (add (Suc x) y) z .=. add (Suc x) (add y z)
  IH:      add (add x y) z       .=. add x (add y z)

  Proof
                       add (add (Suc x) y) z
      (by def add) .=. add (Suc (add x y)) z
      (by def add) .=. Suc (add (add x y) z)
      (by IH)      .=. Suc (add x (add y z))
      (by def add) .=. add (Suc x) (add y z)
  QED
QED
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

Lemma: eval (simp e) .=. eval e
Proof by induction on AExp e

Case (Val i)
  To show: eval (simp (Val i)) .=. eval (Val i)

  Proof
                        eval (simp (Val i))
      (by def simp) .=. eval (Val i)
  QED

Case (Add a b)
  To show: eval (simp (Add a b)) .=. eval (Add a b)
  IH1:     eval (simp a)         .=. eval a
  IH2:     eval (simp b)         .=. eval b

  Proof by case analysis on a == Val 0
  Case True
    Assumption: a == Val 0

    Proof
                            eval (simp (Add a b))
        (by def simp)   .=. eval (if a == Val 0 then b else Add (simp a) (simp b))
        (by Assumption) .=. eval (simp b)
        (by IH2)        .=. eval b
        (by zeroAdd)    .=. 0 + (eval b)
        (by def eval)   .=. (eval (Val 0)) + (eval b)
        (by def eval)   .=. eval (Add (Val 0) b)
        (by Assumption) .=. eval (Add a b)
    QED

  Case False
    Assumption: a /= Val 0

    Proof
                            eval (simp (Add a b))
        (by def simp)   .=. eval (if a == Val 0 then Val b else Add (simp a) (simp b))
        (by Assumption) .=. eval (Add (simp a) (simp b))
        (by def eval)   .=. (eval (simp a)) + (eval (simp b))
        (by IH1)        .=. (eval a) + (eval (simp b))
        (by IH2)        .=. (eval a) + (eval b)
        (by def eval)   .=. eval (Add a b)
    QED
  QED

Case (Mul a b)
  To show: eval (simp (Mul a b)) .=. eval (Mul a b)
  IH1:     eval (simp a)         .=. eval a
  IH2:     eval (simp b)         .=. eval b
  Proof
                           eval (simp (Mul a b))
      (by def simp)    .=. eval (Mul (simp a) (simp b))
      (by def eval)    .=. (eval (simp a)) * (eval (simp b))
      (by IH1)         .=. (eval a) * (eval (simp b))
      (by IH2)         .=. (eval a) * (eval b)
      (by def eval)    .=. eval (Mul a b)
  QED
QED
-}

{-1.3.1
data List a = [] | a : List a

reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

itrev [] xs = xs
itrev (x:xs) ys = itrev xs (x:ys)

[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

axiom app_assoc: (xs ++ ys) ++ zs .=. xs ++ (ys ++ zs)
axiom app_empty: xs ++ [] .=. xs

goal itrev xs [] .=. reverse xs

---

We generalize the property itrev xs [] .=. reverse xs to the following statement:

Lemma gen: itrev xs ys .=. reverse xs ++ ys
Proof by induction on List xs

Case []
  To show: itrev [] ys .=. reverse [] ++ ys

  Proof
                           itrev [] ys
      (by def itrev)   .=. ys

                           reverse [] ++ ys
      (by def reverse) .=. [] ++ ys
      (by def ++)      .=. ys
  QED

Case (x:xs)
  To show: itrev (x:xs) ys .=. reverse (x:xs) ++ ys
  IH:      itrev xs ys     .=. reverse xs ++ ys

  Proof
                           itrev (x:xs) ys
      (by def itrev)   .=. itrev xs (x:ys)
      (by IH)          .=. reverse xs ++ (x:ys)

                           reverse (x:xs) ++ ys
      (by def reverse) .=. (reverse xs ++ [x]) ++ ys
      (by app_assoc)   .=. reverse xs ++ ([x] ++ ys)
      (by def ++)      .=. reverse xs ++ (x : ([] ++ ys))
      (by def ++)      .=. reverse xs ++ (x:ys)
  QED
QED

Our goal then follows:

Lemma: itrev xs [] .=. reverse xs
Proof
                       itrev xs []
    (by gen)       .=. reverse xs ++ []
    (by app_empty) .=. reverse xs
QED
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

Lemma push_a: foldl f (f y a) xs .=. f y (foldr f a xs)
Proof by induction on List xs

Case []
  To show: foldl f (f y a) [] .=. f y (foldr f a [])

  Proof
                         foldl f (f y a) []
      (by def foldl) .=. f y a
      (by def foldr) .=. f y (foldr f a [])
  QED

Case x:xs
  To show: foldl f (f y a) (x:xs) .=. f y (foldr f a (x:xs))
  IH:      foldl f (f y a) xs     .=. f y (foldr f a xs)

  Proof
                         foldl f (f y a) (x:xs)
      (by def foldl) .=. foldl f (f (f y a) x) xs
      (by f_assoc)   .=. foldl f (f y (f a x)) xs
      (by f_comm_a)  .=. foldl f (f y (f x a)) xs
      (by f_assoc)   .=. foldl f (f (f y x) a) xs
      (by IH)        .=. f (f y x) (foldr f a xs)
      (by f_assoc)   .=. f y (f x (foldr f a xs))
      (by def foldr) .=. f y (foldr f a (x:xs))
  QED
QED

Lemma: foldl f a .=. foldr f a
Proof by extensionality with xs
  To show: foldl f a xs .=. foldr f a xs

  Proof by case analysis on List xs
  Case []
    Assumption: xs .=. []

    Proof
                            foldl f a xs
        (by Assumption) .=. foldl f a []
        (by def foldl)  .=. a
        (by def foldr)  .=. foldr f a []
        (by Assumption) .=. foldr f a xs
    QED

  Case y:ys
    Assumption: xs .=. y:ys

    Proof
                            foldl f a xs
        (by Assumption) .=. foldl f a (y:ys)
        (by def foldl)  .=. foldl f (f a y) ys
        (by f_comm_a)   .=. foldl f (f y a) ys
        (by push_a)     .=. f y (foldr f a ys)
        (by def foldr)  .=. foldr f a (y:ys)
        (by Assumption) .=. foldr f a xs
    QED
  QED
QED
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
Lemma: sum2 xs ys .=. sum xs + sum ys
Proof by sum2-induction on xs ys

Case 1
  To show: sum2 [] [] .=. sum [] + sum []

  Proof
                                 sum2 [] []
      (by def sum2)          .=. 0
      (by def sum)           .=. sum []
      (by addZero)           .=. sum [] + 0
      (by def sum)           .=. sum [] + sum []
  QED

Case 2
  To show: sum2 [] (y:ys) .=. sum [] + sum (y:ys)
  IH:      sum2 ys []     .=. sum ys + sum []

  Proof
                                 sum2 [] (y:ys)
      (by def sum2)          .=. y + sum2 ys []
      (by IH)                .=. y + (sum ys + sum [])
      (by addAssoc)          .=. y + sum ys + sum []
      (by def sum)           .=. sum (y:ys) + sum []
      (by addComm)           .=. sum [] + sum (y:ys)
  QED

Case 3
  To show: sum2 (x:xs) ys .=. sum (x:xs) + sum ys
  IH:      sum2 xs ys     .=. sum xs + sum ys

  Proof
                                 sum2 (x:xs) ys
      (by def sum2)          .=. x + sum2 xs ys
      (by IH)                .=. x + (sum xs + sum ys)
      (by addAssoc)          .=. x + sum xs + sum ys
      (by def sum)           .=. sum (x:xs) + sum ys
  QED
QED
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

Lemma sumAddEqSumAux: sum xs + a .=. sum_aux xs a
Proof by induction on List xs

Case []
  To show: sum [] + a .=. sum_aux [] a

  Proof
                           sum [] + a
      (by def sum)     .=. sum_aux [] 0 + a
      (by def sum_aux) .=. 0 + a
      (by zeroAdd)     .=. a
      (by def sum_aux) .=. sum_aux [] a
  QED

Case (x:xs)
  To show: sum (x:xs) + a .=. sum_aux (x:xs) a
  IH:      sum xs + a     .=. sum_aux xs a

  Proof
                          sum (x:xs) + a
      (by def sum)     .=. sum_aux (x:xs) 0 + a
      (by def sum_aux) .=. sum_aux xs (0+x) + a
      (by zeroAdd)     .=. sum_aux xs x + a
      (by IH)          .=. sum xs + x + a

                          sum_aux (x:xs) a
      (by def sum_aux) .=. sum_aux xs (a+x)
      (by IH)          .=. sum xs + (a + x)
      (by addComm)     .=. sum xs + (x + a)
      (by addAssoc)    .=. sum xs + x + a
  QED
QED

Lemma: sum (xs ++ ys) .=. sum xs + sum ys
Proof by induction on List xs
Case []
  To show: sum ([] ++ ys) .=. sum [] + sum ys

  Proof
                           sum ([] ++ ys)
      (by def ++)      .=. sum ys
      (by zeroAdd)     .=. 0 + sum ys
      (by def sum_aux) .=. sum_aux [] 0 + sum ys
      (by def sum)     .=. sum [] + sum ys
  QED

Case x:xs
  To show: sum ((x:xs) ++ ys) .=. sum (x:xs) + sum ys
  IH:      sum (xs ++ ys)     .=. sum xs + sum ys

  Proof
                              sum ((x:xs) ++ ys)
      (by def sum)        .=. sum_aux ((x:xs) ++ ys) 0
      (by def ++)         .=. sum_aux (x:(xs ++ ys)) 0
      (by def sum_aux)    .=. sum_aux (xs ++ ys) (0+x)
      (by zeroAdd)        .=. sum_aux (xs ++ ys) x
      (by sumAddEqSumAux) .=. sum (xs ++ ys) + x
      (by IH)             .=. sum xs + sum ys + x

                              sum (x:xs) + sum ys
      (by def sum)        .=. sum_aux (x:xs) 0 + sum ys
      (by def sum_aux)    .=. sum_aux xs (0+x) + sum ys
      (by zeroAdd)        .=. sum_aux xs x + sum ys
      (by sumAddEqSumAux) .=. sum xs + x + sum ys
      (by addAssoc)       .=. sum xs + (x + sum ys)
      (by addComm)        .=. sum xs + (sum ys + x)
      (by addAssoc)       .=. sum xs + sum ys + x
  QED
QED
-}

{-2.1.2a
data List a = [] | a : List a

reverse [] = []
reverse (x : xs) = snoc (reverse xs) x

snoc [] y = [y]
snoc (x : xs) y = x : snoc xs y

goal reverse (snoc xs x) .=. x : reverse xs

---

Lemma: reverse (snoc xs y) .=. y : reverse xs
Proof by induction on List xs

Case []
  To show: reverse (snoc [] y) .=. y : reverse []

  Proof
                           reverse (snoc [] y)
      (by def snoc)    .=. reverse (y : [])
      (by def reverse) .=. snoc (reverse []) y
      (by def reverse) .=. snoc [] y
      (by def snoc)    .=. [y]
      (by def reverse) .=. y : reverse []
  QED

Case (x:xs)
  To show: reverse (snoc (x:xs) y) .=. y : reverse (x:xs)
  IH:      reverse (snoc xs y)     .=. y : reverse xs

  Proof
                           reverse (snoc (x:xs) y)
      (by def snoc)    .=. reverse (x : snoc xs y)
      (by def reverse) .=. snoc (reverse (snoc xs y)) x
      (by IH)          .=. snoc (y : reverse xs) x
      (by def snoc)    .=. y : snoc (reverse xs) x
      (by def reverse) .=. y : (reverse (x:xs))
  QED
QED
-}

{-2.1.2b
data List a = [] | a : List a

reverse [] = []
reverse (x : xs) = snoc (reverse xs) x

snoc [] y = [y]
snoc (x : xs) y = x : snoc xs y

goal reverse (reverse xs) .=. xs

---

Lemma reverse_snoc: reverse (snoc xs y) .=. y : (reverse xs)
Proof by induction on List xs

Case []
  To show: reverse (snoc [] y) .=. y : (reverse [])

  Proof
                          reverse (snoc [] y)
      (by def snoc)    .=. reverse [y]
      (by def reverse) .=. snoc (reverse []) y
      (by def reverse) .=. snoc [] y
      (by def snoc)    .=. [y]
      (by def reverse) .=. y : (reverse [])
  QED

Case (x:xs)
  To show: reverse (snoc (x:xs) y) .=. y:(reverse (x:xs))
  IH:      reverse (snoc xs y)     .=. y:(reverse xs)

  Proof
                          reverse (snoc (x:xs) y)
      (by def snoc)    .=. reverse (x: snoc xs y)
      (by def reverse) .=. snoc (reverse (snoc xs y)) x
      (by IH)          .=. snoc (y:(reverse xs)) x

                          y : reverse (x:xs)
      (by def reverse) .=. y : snoc (reverse xs) x
      (by def snoc)    .=. snoc (y:(reverse xs)) x
  QED
QED

Lemma: reverse (reverse xs) .=. xs
Proof by induction on List xs

Case []
  To show: reverse (reverse []) .=. []

  Proof
                          reverse (reverse [])
      (by def reverse) .=. reverse ([])
      (by def reverse) .=. []
  QED

Case (x:xs)
  To show: reverse (reverse (x:xs)) .=. x:xs
  IH:      reverse (reverse xs)     .=. xs

  Proof
                          reverse (reverse (x:xs))
      (by def reverse) .=. reverse (snoc (reverse xs) x)
      (by reverse_snoc).=. x : (reverse (reverse xs))
      (by IH)          .=. x : xs
  QED
QED
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

Lemma sum_append: sum (xs ++ ys) .=. sum xs + sum ys
Proof by induction on List xs

Case []
  To show: sum ([] ++ ys) .=. sum [] + sum ys

  Proof
                              sum ([] ++ ys)
      (by def ++)         .=. sum ys
      (by zeroAdd)        .=. 0 + sum ys
      (by def sum)        .=. sum [] + sum ys
  QED

Case x:xs
  To show: sum ((x:xs) ++ ys) .=. sum (x:xs) + sum ys
  IH:      sum (xs ++ ys)     .=. sum xs + sum ys

  Proof
                              sum ((x:xs) ++ ys)
      (by def ++)         .=. sum (x : (xs ++ ys))
      (by def sum)        .=. x + sum (xs ++ ys)
      (by IH)             .=. x + (sum xs + sum ys)
      (by addAssoc)       .=. (x + sum xs) + sum ys
      (by def sum)        .=. sum (x:xs) + sum ys
  QED
QED

Lemma: sum (inorder t) .=. sumTree t
Proof by induction on Tree t

Case Leaf
  To show: sum (inorder Leaf) .=. sumTree Leaf

  Proof
                           sum (inorder Leaf)
      (by def inorder) .=. sum []
      (by def sum)     .=. 0
      (by def sumTree) .=. sumTree Leaf
  QED

Case (Node l x r)
  To show: sum (inorder (Node l x r)) .=. sumTree (Node l x r)
  IH1:     sum (inorder l)            .=. sumTree l
  IH2:     sum (inorder r)            .=. sumTree r

  Proof
                              sum (inorder (Node l x r))
      (by def inorder)    .=. sum ((inorder l) ++ (x : (inorder r)))
      (by sum_append)     .=. sum (inorder l) + sum (x : (inorder r))
      (by def sum)        .=. sum (inorder l) + (x + sum (inorder r))
      (by IH1)            .=. sumTree l + (x + sum (inorder r))
      (by IH2)            .=. sumTree l + (x + sumTree r)
      (by addAssoc)       .=. sumTree l + x + sumTree r
      (by def sumTree)    .=. sumTree (Node l x r)
  QED
QED
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

We generalize the property app t [] .=. flat t to the following statement:

Lemma gen: app t xs .=. flat t ++ xs
Proof by induction on Tree t

Case L
  To show: app L xs .=. flat L ++ xs

  Proof
                        app L xs
      (by def app)  .=. xs
      (by def ++)   .=. [] ++ xs
      (by def flat) .=. flat L ++ xs
  QED

Case (N l x r)
  To show: app (N l x r) xs .=. flat (N l x r) ++ xs
  IH1:     app l xs         .=. flat l ++ xs
  IH2:     app r xs         .=. flat r ++ xs

  Proof
                         app (N l x r) xs
      (by def app)   .=. app l (x : app r xs)
      (by IH1)       .=. flat l ++ (x : app r xs)
      (by IH2)       .=. flat l ++ (x : (flat r ++ xs))

                         flat (N l x r) ++ xs
      (by def flat)  .=. (flat l ++ (x : flat r )) ++ xs
      (by app_assoc) .=. flat l ++ ((x : flat r) ++ xs)
      (by def app)   .=. flat l ++ (x : (flat r ++ xs))
  QED
QED

Our goal then follows:

Lemma: app t [] .=. flat t
Proof
                     app t []
    (by gen)     .=. flat t ++ []
    (by app_nil) .=. flat t
QED
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

Lemma fpfp: filter p (filter p xs) .=. filter p xs
Proof by induction on List xs

Case []
  To show: filter p (filter p []) .=. filter p []

  Proof
                          filter p (filter p [])
      (by def filter) .=. filter p []
  QED

Case (x:xs)
  To show: filter p (filter p (x:xs)) .=. filter p (x:xs)
  IH:      filter p (filter p xs)     .=. filter p xs

  Proof by case analysis on Bool p x
  Case True
    Assumption: p x .=. True

    Proof
                            filter p (filter p (x:xs))
        (by def filter) .=. filter p (if p x then x : filter p xs else filter p xs)
        (by Assumption) .=. filter p (if True then x : filter p xs else filter p xs)
        (by if_True)    .=. filter p (x : filter p xs)
        (by def filter) .=. if p x then x : filter p (filter p xs) else filter p (filter p xs)
        (by Assumption) .=. if True then x : filter p (filter p xs) else filter p (filter p xs)
        (by if_True)    .=. x : filter p (filter p xs)
        (by IH)         .=. x : filter p xs

                            filter p (x:xs)
        (by def filter) .=. if p x then x : filter p xs else filter p xs
        (by Assumption) .=. if True then x : filter p xs else filter p xs
        (by if_True)    .=. x : filter p xs
    QED

  Case False
    Assumption: p x .=. False

    Proof
                            filter p (filter p (x:xs))
        (by def filter) .=. filter p (if p x then x : filter p xs else filter p xs)
        (by Assumption) .=. filter p (if False then x : filter p xs else filter p xs)
        (by if_False)   .=. filter p (filter p xs)
        (by IH)         .=. filter p xs

                            filter p (x:xs)
        (by def filter) .=. if p x then x : filter p xs else filter p xs
        (by Assumption) .=. if False then x : filter p xs else filter p xs
        (by if_False)   .=. filter p xs
    QED
  QED
QED

Lemma: filter p . filter p .=. filter p
Proof by extensionality with xs
  To show: (filter p . filter p) xs .=. filter p xs

  Proof
                     (filter p . filter p) xs
      (by def .) .=. filter p (filter p xs)
      (by fpfp)  .=. filter p xs
  QED
QED
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

Lemma: length (drop2 xs) .=. (length xs + 1) `div` 2
Proof by drop2-induction on xs
Case 1
  To show: length (drop2 []) .=. (length [] + 1) `div` 2

  Proof
                          length (drop2 [])
      (by def drop2)    .=. length []
      (by def length)   .=. 0

                           (length [] + 1) `div` 2
      (by def length)   .=. (0 + 1) `div` 2
      (by addZeroOne)   .=. 1 `div` 2
      (by divOneTwo)    .=. 0
  QED

Case 2
  To show: length (drop2 [x]) .=. (length [x] + 1) `div` 2

  Proof
                           length (drop2 [x])
      (by def drop2)    .=. length [x]
      (by def length)   .=. 1 + length []
      (by def length)   .=. 1 + 0
      (by addOneZero)   .=. 1

                          (length [x] + 1) `div` 2
      (by def length)   .=. ((1 + length []) + 1) `div` 2
      (by def length)   .=. ((1 + 0) + 1) `div` 2
      (by addOneZero)   .=. (1 + 1) `div` 2
      (by addOneOne)    .=. 2 `div` 2
      (by divTwoTwo)    .=. 1
  QED

Case 3
  To show: length (drop2 (x:y:xs)) .=. (length (x:y:xs) + 1) `div` 2
  IH:      length (drop2 xs)       .=. (length xs + 1) `div` 2

  Proof
                           length (drop2 (x:y:xs))
      (by def drop2)    .=. length (x : drop2 xs)
      (by def length)   .=. 1 + length (drop2 xs)
      (by IH)           .=. 1 + ((length xs + 1) `div` 2)
      (by divMulOneTwo) .=. ((length xs + 1) + 2) `div` 2
      (by def length)   .=. (length (y:xs) + 2) `div` 2
      (by addOneOne)    .=. (length (y:xs) + (1 + 1)) `div` 2
      (by addAssoc)     .=. ((length (y:xs) + 1) + 1) `div` 2
      (by def length)   .=. (length (x:y:xs) + 1) `div` 2
  QED
QED
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

Lemma: countGt xs ys <= length xs + length ys
Proof by countGt-induction on xs ys
Case 1
  To show: countGt [] ys <= length [] + length ys

  Proof
                                         countGt [] ys
      (by def countGt)               .=. 0
      (by length)                    .=. length []
      (by addZero)                   .=. length [] + 0
      (by leAddMono OF lengthNonneg) <=  length [] + length ys
  QED

Case 2
  To show: countGt (x:xs) [] <= length (x:xs) + length []

  Proof
                                        countGt (x:xs) []
    (by def countGt)                .=. length (x:xs)
    (by zeroAdd)                    .=. length (x:xs) + 0
    (by leAddMono OF lengthNonneg)  <=  length (x:xs) + length []
  QED

Case 3
  To show: countGt (x:xs) (y:ys) <= length (x:xs) + length (y:ys)
  IH1: x > y .=. True ==> countGt (x:xs) ys <= length (x:xs) + length ys
  IH2: x > y .=. False ==> countGt (y:ys) xs <= length (y:ys) + length xs

  Proof by case analysis on Bool (x > y)
  Case True
    Assumption: (x > y) .=. True
    Proof
                                 countGt (x:xs) (y:ys)
      (by def countGt)       .=. if x > y then 1 + countGt (x:xs) ys else countGt (y:ys) xs
      (by Assumption)        .=. if True then 1 + countGt (x:xs) ys else countGt (y:ys) xs
      (by ifTrue)            .=. 1 + countGt (x:xs) ys
      (by leAddMono OF (IH1 OF Assumption))
                             <=  1 + (length (x:xs) + length ys)
      (by addComm)           .=. 1 + (length ys + length (x:xs))
      (by addAssoc)          .=. 1 + length ys + length (x:xs)
      (by def length)        .=. length (y:ys) + length (x:xs)
      (by addComm)           .=. length (x:xs) + length (y:ys)
    QED
  Case False
    Assumption: (x > y) .=. False
    Proof
                                       countGt (x:xs) (y:ys)
      (by def countGt)             .=. if x > y then 1 + countGt (x:xs) ys else countGt (y:ys) xs
      (by Assumption)              .=. if False then 1 + countGt (x:xs) ys else countGt (y:ys) xs
      (by ifFalse)                 .=. countGt (y:ys) xs
      (by IH2 OF Assumption)       <=  length (y:ys) + length xs
      (by addZero)                 .=. length (y:ys) + length xs + 0
      (by leAddMono OF zeroLeOne)  <=  length (y:ys) + length xs + 1
      (by addAssoc)                .=. length (y:ys) + (length xs + 1)
      (by def length)              .=. length (y:ys) + length (x:xs)
      (by addComm)                 .=. length (x:xs) + length (y:ys)
    QED
  QED
QED
-}
