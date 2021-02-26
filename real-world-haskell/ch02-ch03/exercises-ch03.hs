-- ch03 exercises
import Data.List

-- 1. implement length
myLength :: [a] -> Int
myLength []     = 0
myLength (x:xs) = 1 + myLength xs

-- 3. calculate the mean of a list
meanlist []     = 0
meanlist (x:xs) = sum (x:xs) / fromIntegral (myLength (x:xs))

-- 4. turn a list into a palindrome
palindromize []     = []
palindromize (x:xs) = x : palindromize xs ++ [x]

-- 5. determine if a list is a palindrome
isPalindrome xs = xs == reverse xs

-- 6. sort a list of lists based on the length of each sublist
sublistSort :: [[a]] -> [[a]]
sublistSort = sortBy compLen
    where compLen a b = compare (length a)  (length b)

-- 7. join a list of lists together using a seperator value
myIntersperse :: a -> [[a]] -> [a]
myIntersperse _   []     = []
myIntersperse _   (x:[]) = x
myIntersperse sep (x:xs) = x ++ [sep] ++ myIntersperse sep xs

-- 8. find the height of a binary tree. Binary Tree type from notes.hs
data BinaryTree a = Node a (BinaryTree a) (BinaryTree a)
                  | Empty
                    deriving (Show)

measureHeight :: BinaryTree a -> Int
measureHeight Empty                    = 0
measureHeight (Node _ r l)
    | rightTreeHeight > leftTreeHeight = 1 + rightTreeHeight
    | otherwise                        = 1 + leftTreeHeight
    where rightTreeHeight = measureHeight r
          leftTreeHeight  = measureHeight l

-- 9. define Direction datatype that represents the direction of an angle
--    between 2 points
data Direction = Clockwise | CounterClockwise | Straight
                 deriving (Show)

-- 10. calculate the Direction of a turn made by 3 2D points
data Point2D = Point2D Double Double
               deriving Show

calcTurn (Point2D x y) (Point2D z w) (Point2D u v)
    | angle == pi    = Straight
    | angle == (-pi) = Straight
    | angle == 0     = Straight
    | angle > 0      = Clockwise
    | angle < 0      = CounterClockwise
    where dotProduct      = (x - z) * (u - z) + (y - w) * (u - w)
          crossProductLen = (x - z) * (u - w) - (y - w) * (u - z)
          angle           = atan2 crossProductLen dotProduct

-- 11 & 12 have more to do with vector math than haskell 
