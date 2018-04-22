module Tree

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

Functor Tree where
  map func Empty = Empty
  map func (Node left val right)
      = Node (map func left)
             (func val)
             (map func right)

Foldable Tree where
  foldr func init Empty = init
  foldr func init (Node left val right)
        = let leftFold = foldr func init left
              rightFold = foldr func leftFold right in
              func val rightFold
