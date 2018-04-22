module Vect

data Vect : (len : Nat) -> (elem : Type) -> Type where
     Nil : Vect Z elem
     (::) : (x : elem) -> (xs : Vect len elem) -> Vect (S len) elem

-- 3.2
Eq ty => Eq (Vect n ty) where
  (==) [] [] = True
  (==) (x :: xs) (y :: ys) = x == y && xs == ys
  (==) _ _ = False

Foldable (Vect n) where
  foldr func init [] = init
  foldr func init (x :: xs) = func x (foldr func init xs)
  
  foldl func init [] = init
  foldl func init (x :: xs) = foldl func (func init x) xs
