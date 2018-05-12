module RemoveElem

import Data.Vect

-- Playing around with Elem
fooInVector : Elem "Foo" ["Bar", "Baz", "Foo"]
fooInVector = There $ There Here

removeElem : (value : a) -> (xs : Vect (S n) a) ->
             {auto prf : Elem value xs} -> Vect n a
removeElem value (value :: ys) {prf = Here} = ys
removeElem {n = Z} value (y :: []) {prf = (There later)} = absurd later
removeElem {n = (S k)} value (y :: ys) {prf = (There later)}
           = y :: removeElem value ys
