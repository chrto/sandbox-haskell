data Trie a = Leaf a | Node a [Trie a]

myTrie :: Trie Char
myTrie =
  Node 'c' [Node 'a' [Leaf 'r', Leaf 't'], Node 'o' [Node 'o' [Leaf 'l']]]

foldtrie :: (b -> a -> b) -> b -> Trie a -> b
foldtrie f acc (Leaf c   ) = f acc c
foldtrie f acc (Node c ns) = foldl (foldtrie f) (f acc c) ns

-- foldtrie :: (b -> a -> b) -> b -> Trie a -> b
-- foldtrie f acc (Leaf c         ) = f acc c
-- foldtrie f acc (Node c []      ) = f acc c
-- foldtrie f acc (Node c (n : ns)) = foldtrie' f' acc' ns
--  where
--   -- f' :: b -> Trie a -> b
--   f'   = foldtrie f

--   -- acc' :: b
--   acc' = foldtrie f (f acc c) n

--   foldtrie' :: (b -> Trie a -> b) -> b -> [Trie a] -> b
--   foldtrie' f'' v []         = v
--   foldtrie' f'' v (tr : trs) = foldtrie' f'' (f'' v tr) trs


test :: Trie Char -> [Char]
test = foldtrie (\acc x -> acc ++ [x]) []
