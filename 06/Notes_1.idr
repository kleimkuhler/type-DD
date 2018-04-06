module Notes_1

import Data.Vect

Position : Type
Position = (Double, Double)

Polygon : Nat -> Type
Polygon k = Vect k Position

tri : Polygon 3
tri = [(0.0, 0.0), (3.0, 0.0), (0.0, 4.0)]

StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int

getStringOrInt : (isInt : Bool) -> StringOrInt isInt
getStringOrInt False = "Ninety four"
getStringOrInt True = 94

valToString : (isInt : Bool) -> (case isInt of
                                      False => String
                                      True => Int) -> String
valToString False x = trim x
valToString True x = cast x
