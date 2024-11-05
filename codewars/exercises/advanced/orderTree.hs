import           Data.List                      ( sort )

data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving (Show)

emptyTree :: Tree a
emptyTree = Leaf

node1 = Node Leaf 1 Leaf
node2 = Node node1 2 node3
node3 = Node Leaf 3 Leaf
node4 = Node node2 4 node5
node5 = Node Leaf 5 node6
node6 = Node Leaf 6 Leaf

insert :: (Ord a) => a -> Tree a -> Tree a
insert v Leaf = Node Leaf v Leaf
insert v (Node left n right) | v < n     = Node (insert v left) n right
                             | v > n     = Node left n (insert v right)
                             | otherwise = Node left v right

inorder :: Tree a -> [a]
inorder Leaf                = []
inorder (Node left n right) = inorder left ++ [n] ++ inorder right


prop :: Ord a => a -> Tree a -> Bool
prop v tree = inorder (insert v tree) == sort (v : inorder tree)
