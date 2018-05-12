module Exercises_1

-- 1
data Elem : a -> List a -> Type where
     Here : Elem x (x :: xs)
     There : (later : Elem x xs) -> Elem x (y :: xs)

-- 2
data Last : List a -> a -> Type where
     LastOne : Last [value] value
     LastCons : (prf : Last xs value) -> Last (x :: xs) value

-- Sample
last123 : Last [1,2,3] 3
last123 = LastCons (LastCons LastOne)

notInNil : Last [] value -> Void
notInNil LastOne impossible
notInNil (LastCons _) impossible

exhaustiveLast : (notLast : (x = value) -> Void) -> Last [x] value -> Void
exhaustiveLast notLast LastOne = notLast Refl
exhaustiveLast notLast (LastCons prf) = notInNil prf

notLastCons : (notLast : Last (y :: xs) value -> Void) ->
              Last (x :: (y :: xs)) value -> Void
notLastCons notLast (LastCons prf) = notLast prf

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No notInNil
isLast (x :: []) value = case decEq x value of
                              Yes Refl => Yes LastOne
                              No notLast => No $ exhaustiveLast notLast
isLast (x :: (y :: xs)) value = case isLast (y :: xs) value of
                                     Yes last => Yes $ LastCons last
                                     No notLast => No $ (notLastCons notLast)
