module Exercises

data BSTree : Type -> Type where
  Empty : Ord elem => BSTree elem
  Node : Ord elem => (left : BSTree elem) -> (val : elem) ->
                     (right : BSTree elem) -> BSTree elem

insert : elem -> BSTree elem -> BSTree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right)
       = case compare x val of
              LT => Node (insert x left) val right
              EQ => orig
              GT => Node left val (insert x right)

data Shape = ||| A triangle
             Triangle Double Double
           | ||| A rectangle
             Rectangle Double Double
           | ||| A circle
             Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

-- 1
listToTree : Ord a => List a -> BSTree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

listToTree' : Ord a => List a -> BSTree a
listToTree' = foldr insert Empty


-- 2
treeToList : BSTree a -> List a
treeToList Empty = []
treeToList (Node left val right) = treeToList left ++ (val :: treeToList right)

-- 3
data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr

-- 4
evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = evaluate x + evaluate y
evaluate (Sub x y) = evaluate x - evaluate y
evaluate (Mult x y) = evaluate x * evaluate y

-- 5
maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing (Just y) = Just y
maxMaybe (Just x) Nothing = Just x
maxMaybe (Just x) (Just y) = Just (max x y)

-- 6
testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))


biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive triangle@(Triangle base height))
                = Just (area triangle)
biggestTriangle (Primitive _) = Nothing
biggestTriangle (Combine pic1 pic2) = maxMaybe (biggestTriangle pic1)
                                               (biggestTriangle pic2)
biggestTriangle (Rotate _ pic) = biggestTriangle pic
biggestTriangle (Translate _ _ pic) = biggestTriangle pic
