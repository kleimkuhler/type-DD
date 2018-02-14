import Data.Vect

describeList : List Int -> String
describeList [] = "Empty"
describeList (x :: xs) = "Non-empty, tail = " ++ show xs

allLengths_List : List String -> List Nat
allLengths_List [] = []
allLengths_List (word :: words) = length word :: allLengths_List words

xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y

mutual
  isEven : Nat -> Bool
  isEven Z = True
  isEven (S k) = isOdd k

  isOdd : Nat -> Bool
  isOdd Z = False
  isOdd (S k) = isEven k

fourInts : Vect 4 Int
fourInts = [0, 1, 2, 3]

allLengths_Vect : Vect len String -> Vect len Nat
allLengths_Vect [] = []
allLengths_Vect (word :: words) = length word :: allLengths_Vect words

insert : Ord elem =>
         (x : elem) -> (xsSorted : Vect len elem) -> Vect (S len) elem
insert x [] = [x]
insert x (y :: xs) = case x < y of
                          False => y :: insert x xs
                          True => x :: y :: xs

insSort : Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs in
                        insert x xsSorted

