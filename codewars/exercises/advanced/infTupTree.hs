module Exercises.Advanced.TupTree where

data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving (Show)

infTupTree :: Tree (Integer, Integer)
infTupTree = buildTree (0, 0)
 where
  buildTree :: (Integer, Integer) -> Tree (Integer, Integer)
  buildTree (x, y) = Node (buildTree (x + 1, y)) (x, y) (buildTree (x, y + 1))

cat :: Integer -> Tree a -> Tree a
cat 0 _ = Leaf
cat n Leaf = Leaf
cat n (Node left a right) = Node (cat (n-1) left) a (cat (n-1) right)
