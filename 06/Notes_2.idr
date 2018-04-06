module Notes_2.idr

AdderType : Nat -> Type -> Type
AdderType Z numType = numType
AdderType (S k) numType = (next : numType) -> AdderType k numType

-- (next + acc)
adder : Num numType =>
        (numargs: Nat) -> numType -> AdderType numargs numType
adder Z acc = acc
adder (S k) acc = \next => adder k (next + acc)
