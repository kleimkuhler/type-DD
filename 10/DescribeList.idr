module DescribeList

data ListLast : List a -> Type where
     Empty : ListLast []
     NonEmpty : (xs : List a) -> (x : a) -> ListLast (xs ++ [x])

describeHelper : (input : List Int) -> ListLast input -> String
describeHelper [] Empty = "Empty"
describeHelper (xs ++ [x]) (NonEmpty xs x) = "Non-empty, initial portion = " ++ show xs

total
listLast : (input : List a) -> ListLast input
listLast [] = Empty
listLast (x :: xs) = case listLast xs of
                          Empty => NonEmpty [] x
                          NonEmpty ys y => NonEmpty (x :: ys) y

describeListEnd : List Int -> String
describeListEnd xs = describeHelper xs (listLast xs)

describeListEnd' : List Int -> String
describeListEnd' input with (listLast input)
  describeListEnd' [] | Empty = "Empty"
  describeListEnd' (xs ++ [x]) | (NonEmpty xs x) 
                   = "Non-empty, initial portion = " ++ show xs

reverse' : List a -> List a
reverse' input with (listLast input)
  reverse' [] | Empty = []
  reverse' (xs ++ [x]) | (NonEmpty xs x) = x :: reverse' xs
