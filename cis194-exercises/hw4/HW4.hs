module HW4 where

fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

-- if the number is even, add the number to fun2 (itself divided by 2)
-- else call fun2 on 3 * n + 1
-- fun2 4 = 4 + fun2 2
--        = 4 + 1 + fun2 1
--        = 4 + 1 + 0 = 5
-- fun2 5 = fun2 16
--        = 16 + fun2 8
--        = 16 + 8 + fun2 4
--        = 16 + 8 + 4 + fun2 2
--        = 16 + 8 + 4 + 2 + fun2 1
--        = 16 + 8 + 4 + 2 + 0 = 30

-- fun2 6 = 6 + fun2 3
--        = 6 + fun2 10
--        = 6 + 10 + fun2 5


data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

height :: Tree a -> Integer
height (Node h _ _ _) = h

-- foldTree takes a list and returns a balanced binary tree of the elements of
-- the list.
{-
foldTree :: [a] -> Tree a
foldTree = foldr addNode Leaf
  where addNode x Leaf              = Node 0 Leaf x Leaf
        addNode x (Node _ Leaf v t) = Node 1 (Node 0 Leaf v Leaf) x t
        addNode x (Node _ t v Leaf) = Node 1 t x (Node 0 Leaf v Leaf)
        addNode x (Node h t1 v t2)
          | height t1
-}

-- xor takes a list of Bools and returns True if there is an odd number of True
-- values in the list.
xor :: [Bool] -> Bool
xor = not . even . foldr count 0
  where count x acc = acc + fromEnum x

-- map' is map implemented using foldr
map' :: (a -> b) -> [a] -> [b]
map' f = foldr f' []
  where f' e es = f e : es

-- cartProd computes the cartesian product of 2 lists
cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

-- sieveSundaram takes an Integer n and returns a list of all odd prime numbers
-- up to 2n+1 using the Sieve Sundaram algorithm.
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map increment . filter (wellFormed components) $ allNats
  where increment m        = 2 * m + 1
        components         = filter properPieces . cartProd allNats $ allNats
        properPieces (i,j) = i <= j && calcForm (i,j) <= n
        wellFormed cs m    = not . any ((== m) . calcForm) $ cs
        calcForm (i,j)     = i + j + 2 * i * j
        allNats            = [1..n]
