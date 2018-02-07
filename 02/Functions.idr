module Functions

import Average

double : Num ty => ty -> ty
double x = x + x

identity : ty -> ty
identity x = x

twice : (a -> a) -> a -> a
twice f x = f (f x)

quadruple : Num ty => ty -> ty
quadruple = twice double

squareTwice : Num ty => ty -> ty
squareTwice = twice (\x => x * x)

longer : String -> String -> Nat
longer str1 str2
  = let len1 = length str1
        len2 = length str2 in
        if len1 > len2 then len1 else len2

pythagoras : Double -> Double -> Double
pythagoras x y = sqrt (square x + square y)
  where
    square : Double -> Double
    square x = x * x
