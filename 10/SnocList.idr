module SnocList

data SnocList : List a -> Type where
     Empty : SnocList []
     Snoc : (rec : SnocList xs) -> SnocList (xs ++ [x])

snocListHelper : (snoc : SnocList input) -> (rest : List a) ->
                 SnocList (input ++ rest)
snocListHelper {input} snoc [] = rewrite appendNilRightNeutral input in snoc
snocListHelper {input} snoc (x :: xs)
               = rewrite appendAssociative input [x] xs in
                         snocListHelper (Snoc snoc {x}) xs

snocList : (input : List a) -> SnocList input
snocList input = snocListHelper Empty input

total
reverse' : List a -> List a
reverse' input with (snocList input)
  reverse' [] | Empty = []
  reverse' (xs ++ [x]) | (Snoc rec) = x :: reverse' xs | rec

total
isSuffix : Eq a => List a -> List a -> Bool
isSuffix input1 input2 with (snocList input1)
  isSuffix [] input2 | Empty = True
  isSuffix (xs ++ [x]) input2 | (Snoc xsrec) with (snocList input2)
    isSuffix (xs ++ [x]) [] | (Snoc xsrec) | Empty = False
    isSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc xsrec) | (Snoc ysrec)
             = if x == y then isSuffix xs ys | xsrec | ysrec
                         else False


