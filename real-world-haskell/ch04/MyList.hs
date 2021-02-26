-- Rewriting the basic functions in the Data.List module

-- length returns the length of a list
myLength []     = 0
myLength (x:xs) = 1 + myLength xs

-- null determines if a list is empty
myNull [] = True
myNull _  = False

-- Partial functions don't have return values for all values
-- Total functions return valid results over entire input domain
myHead (x:xs) = x
myTail (x:xs) = xs
-- etc

-- ++ (append) concatenates the given lists, used as an infix op
[]     `myAppend` ys = ys
(x:xs) `myAppend` ys = x:(xs `myAppend` ys)

-- concat takes a list of lists and concatenates them into one list
myConcat :: [[a]] -> [a]
myConcat []          = []
myConcat ([]:xs)     = myConcat xs
myConcat ((y:ys):xs) = y:myConcat (ys:xs)

-- reverse returns the elements of a list in reverse
myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = revAcc xs []
    where revAcc [] ys     = ys
          revAcc (x:xs) ys = revAcc xs (x:ys)

-- and take a list of Bools and returns True if all are True, False otherwise
myAnd :: [Bool] -> Bool
myAnd []         = True
myAnd (True:xs)  = myAnd xs
myAnd (False:xs) = False

-- or takes a list of Bools and returns True if any are True, False otherwise
myOr :: [Bool] -> Bool
myOr []         = False
myOr (True:xs)  = True
myOr (False:xs) = myOr xs

-- all takes a predicate and a list and returns True if all satisfy the pred
myAll :: (a -> Bool) -> [a] -> Bool
myAll p []      = True
myAll p (x:xs)
    | p x       = myAll p xs
    | otherwise = False

-- any takes a predicate and a list and returns True if any satisfy the pred
myAny :: (a -> Bool) -> [a] -> Bool
myAny p []      = False
myAny p (x:xs)
    | p x       = True
    | otherwise = myAny p xs

-- take takes an int k and a list and returns the first k elts of the list
myTake :: Int -> [a] -> [a]
myTake _ []     = []
myTake k (x:xs)
    | k <= 0    = []
    | otherwise = x:myTake (k - 1) xs

-- drop takes an int k and a list and returns a list without the first k elts
myDrop :: Int -> [a] -> [a]
myDrop _ []     = []
myDrop k (x:xs)
    | k <= 0    = (x:xs)
    | otherwise = myDrop (k - 1) xs

-- splitAt takes an int k and a list and returns a pair of list split at k
mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt k xs = (myTake k xs, myDrop k xs)

-- takeWhile takes elements from the beginning of the list until p is False
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ []     = []
myTakeWhile p (x:xs)
    | p x            = x:myTakeWhile p xs
    | otherwise      = []

-- dropWhile removes elements from the beginning of the list until p is False
myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile _ []     = []
myDropWhile p (x:xs)
    | p x            = myDropWhile p xs
    | otherwise      = x:xs

-- span splits a list into 2 and traverses until p returns False
mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan p xs = (myTakeWhile p xs, myDropWhile p xs)

-- break splits a list into 2 and traverses until p returns True
myBreak :: (a -> Bool) -> [a] -> ([a], [a])
myBreak p xs = (myTakeWhile (not . p) xs, myDropWhile (not . p) xs)

-- elem indicates whether a value is present in a list
myElem :: (Eq a) => a -> [a] -> Bool
myElem _ []     = False
myElem e (x:xs)
    | x == e    = True
    | otherwise = myElem e xs

-- filter takes a predicate and returns every elt of the list that rets True
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ []     = []
myFilter p (x:xs)
    | p x         = x:myFilter p xs
    | otherwise   = myFilter p xs

-- isPrefixOf takes 2 lists and determines if the 2nd list starts with the 1st
myIsPrefixOf :: (Eq a) => [a] -> [a] -> Bool
myIsPrefixOf []     _      = True
myIsPrefixOf _      []     = False
myIsPrefixOf (x:xs) (y:ys)
    | x == y               = myIsPrefixOf xs ys
    | otherwise            = False

-- isInfixOf takes 2 lists and determines if the 2nd list contains the 1st
myIsInfixOf :: (Eq a) => [a] -> [a] -> Bool
myIsInfixOf []     _         = True
myIsInfixOf _      []        = False
myIsInfixOf xs (y:ys)
    | myIsPrefixOf xs (y:ys) = True
    | otherwise              = myIsInfixOf xs ys

-- isSuffixOf takes 2 lists and determines if the 2nd list ends with the 1st
myIsSuffixOf :: (Eq a) => [a] -> [a] -> Bool
myIsSuffixOf []  _     = True
myIsSuffixOf _  []     = False
myIsSuffixOf xs (y:ys)
    | xs == (y:ys)     = True
    | otherwise        = myIsSuffixOf xs ys

-- zip takes 2 lists and returns a list of pairs of corresponding elts
myZip :: [a] -> [b] -> [(a, b)]
myZip (x:xs) (y:ys) = (x, y):myZip xs ys
myZip _      _      = []

-- zipWith takes a function and 2 lists, applying f to corresponding elts
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f (x:xs) (y:ys) = f x y:myZipWith f xs ys
myZipWith _ _      _      = []
