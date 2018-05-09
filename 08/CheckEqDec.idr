module CheckEqDec

%default total

noRec : (contra : (k = j) -> Void) -> (S k = S j) -> Void
noRec contra Refl = contra Refl

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Dec (num1 = num2)
checkEqNat Z Z = Yes Refl
checkEqNat Z (S k) = No ZnotS
checkEqNat (S k) Z = No SIsNotZ
checkEqNat (S k) (S j) = case checkEqNat k j of
                              (Yes prf) => Yes $ cong prf
                              (No contra) => No (noRec contra)
