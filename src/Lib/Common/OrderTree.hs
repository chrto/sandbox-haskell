module Lib.Common.OrderTree where

import           Data.List (sort)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import           Test.QuickCheck.Gen (Gen, frequency, suchThat)

data Tree a = Leaf
            | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

arbitraryTree :: (Ord a, Arbitrary a) => Gen (Tree a)
arbitraryTree = do
  x <- arbitrary
  y <- suchThat arbitrary (>= x)
  go x y
  where
    go mn mx = frequency [(1, return Leaf), (1, arbNode)]
      where
        arbNode = do
          a <- suchThat arbitrary (\x -> x >= mn && x <= mx)
          l <- go mn a
          r <- go a mx
          return (Node l a r)

instance (Ord a, Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = arbitraryTree

emptyTree :: Tree a
emptyTree = Leaf

node1 :: Tree Integer
node1 = Node Leaf 1 Leaf

node2 :: Tree Integer
node2 = Node node1 2 node3

node3 :: Tree Integer
node3 = Node Leaf 3 Leaf

node4 :: Tree Integer
node4 = Node node2 4 node5

node5 :: Tree Integer
node5 = Node Leaf 5 node6

node6 :: Tree Integer
node6 = Node Leaf 6 Leaf

insert :: (Ord a) => a -> Tree a -> Tree a
insert v Leaf = Node Leaf v Leaf
insert v (Node left n right)
  | v <= n = Node (insert v left) n right
  | otherwise = Node left n (insert v right)

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node left n right) = inorder left ++ [n] ++ inorder right

advProp :: Integer -> Tree Integer -> Bool
advProp v tree = inorder (insert v tree) == sort (v:inorder tree)
------------------------
-- arbitraryTree :: (Ord a, Arbitrary a) => Gen (Tree a)
-- arbitraryTree = do
--   x <- arbitrary
--   y <- suchThat arbitrary (>= x)
--   go x y
--  where
--   go mn mx = frequency [(1, return Leaf), (1, arbNode)]
--    where
--     arbNode = do
--       a <- suchThat arbitrary (\x -> x >= mn && x <= mx)
--       l <- go mn a
--       r <- go a mx
--       return (Node l a r)
-- instance (Ord a, Arbitrary a) => Arbitrary (Tree a) where
--   arbitrary = arbitraryTree
-- insert :: (Ord a) => a -> Tree a -> Tree a
-- insert v Leaf = Node Leaf v Leaf
-- insert v (Node left n right) | v < n     = Node (insert v left) n right
--                              | v > n     = Node left n (insert v right)
--                              | otherwise = Node left v right
-- inorder :: Tree a -> [a]
-- inorder Leaf                = []
-- inorder (Node left n right) = inorder left ++ [n] ++ inorder right
-- advProp :: [Integer] -> Bool
-- advProp xs = sort (nub xs) == inorder (foldr insert Leaf xs)
