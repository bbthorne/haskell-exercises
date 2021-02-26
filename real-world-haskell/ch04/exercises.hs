import Data.Char (digitToInt, isDigit)
import Data.List (foldl')
-- 2. splitWith takes a predicate and a list, splitting the list on every elt
--    for which the predicate returns False.
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f (x:xs) = first:splitWith f rest
    where (first, rest) = span f (x:xs)
splitWith f _      = []

-- 1. asInt converts a string representation of an integer to an integer
asInt :: String -> Int
asInt ('-':xs) = foldl' (convertDigit (-)) 0 xs
asInt xs       = foldl' (convertDigit (+)) 0 xs

convertDigit op num c = op (num * 10) (digitToInt c)

-- 2. asInt_either handles errors
type ErrorMessage = String

asInt_either :: String -> Either ErrorMessage Int
asInt_either []     = Right 0
asInt_either (x:xs) =
    case x of
        '-' -> foldl' (convertDigitEither (-)) (Right 0) xs
        _   -> foldl' (convertDigitEither (+)) (Right 0) (x:xs)
    where convertDigitEither _  (Left x)  _ = Left x
          convertDigitEither op (Right x) c
              | isDigit c = Right (op (x * 10) (digitToInt c))
              | otherwise = Left ('\'' : c : "' is not a digit")

-- 3. concat with foldr
concat_foldr :: [[a]] -> [a]
concat_foldr = foldr (++) []

-- 4. takeWhile with foldr. goes backwards so it only records the longest prefix
takeWhile_foldr :: (a -> Bool) -> [a] -> [a]
takeWhile_foldr p = foldr check []
    where check e acc
              | p e       = e : acc
              | otherwise = []

-- 5. groupBy
myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy comp = foldr groupUp []
    where groupUp e []          = [[e]]
          groupUp e ([]:ys)     = [e]:ys
          groupUp e ((x:xs):ys)
              | comp e x        = (e : x : xs):ys
              | otherwise       = [e]:(x:xs):ys
