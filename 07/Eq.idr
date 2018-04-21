module Eq

data Matter = Solid | Liquid | Gas
data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

Eq Matter where
  (==) Solid Solid = True
  (==) Liquid Liquid = True
  (==) Gas Gas = True
  (==) _ _ = False

Eq elem => Eq (Tree elem) where
   (==) Empty Empty = True
   (==) (Node left val right) (Node left' val' right')
        = left == left' && val == val' && right == right'
   (==) _ _ = False

occurrences : Eq ty => (item : ty) -> (values : List ty) -> Nat
occurrences item [] = 0
occurrences item (x :: xs) = case x == item of
                                  False => occurrences item xs
                                  True => 1 + occurrences item xs
