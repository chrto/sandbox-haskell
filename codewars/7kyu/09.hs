-- compas
data Direction = N | NE | E | SE | S | SW | W | NW
    deriving (Eq, Show, Enum)

data Compas = Node Compas Direction Compas

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

direction :: Direction -> Int -> Direction
direction facing turn = facingCompas compas facing
                          |> turnCompas (getTurn turn)
 where
  compas :: Compas
  compas = nodeN
   where
    nodeN  = Node nodeNW N nodeNE
    nodeNE = Node nodeN NE nodeE
    nodeE  = Node nodeNE E nodeSE
    nodeSE = Node nodeE SE nodeS
    nodeS  = Node nodeSE S nodeSW
    nodeSW = Node nodeS SW nodeW
    nodeW  = Node nodeSW W nodeNW
    nodeNW = Node nodeW NW nodeN

  getTurn :: Int -> Int
  getTurn = flip mod 8 . flip div 45

  facingCompas :: Compas -> Direction -> Compas
  facingCompas (Node prev n next) d | d == n    = Node prev n next
                                    | otherwise = facingCompas next d

  turnCompas :: Int -> Compas -> Direction
  turnCompas x (Node prev n next) | x > 0     = turnCompas (x - 1) next
                                  | x < 0     = turnCompas (x + 1) prev
                                  | otherwise = n
--------------------

direction'' :: Direction -> Int -> Direction
direction'' facing turn =
  head $ moveDirection (setDirection compas facing) $ (turn `div` 45) `mod` 8
 where
  compas :: [Direction]
  compas = [N, NE, E, SE, S, SW, W, NW]

  setDirection :: [Direction] -> Direction -> [Direction]
  setDirection ds x | head ds == x = ds
                    | otherwise    = setDirection (moveRight ds) x

  moveDirection :: [Direction] -> Int -> [Direction]
  moveDirection ds n | n > 0     = moveDirection (moveRight ds) (n - 1)
                     | n < 0     = moveDirection (moveLeft ds) (n + 1)
                     | otherwise = ds

  moveRight :: [Direction] -> [Direction]
  moveRight xs = tail xs ++ [head xs]

  moveLeft :: [Direction] -> [Direction]
  moveLeft xs = last xs : init xs

--------------------

direction' :: Direction -> Int -> Direction
direction' facing turn = toEnum $ (fromEnum facing + turn `div` 45) `mod` 8
