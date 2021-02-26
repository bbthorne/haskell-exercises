import Data.Char (digitToInt)
-- Use "acc'" to indicate that acc has changed and that's the new val
asInt :: String -> Int
asInt xs = loop 0 xs
    where loop acc []     = acc
          loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                            in loop acc' xs
