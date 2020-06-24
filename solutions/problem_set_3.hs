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
