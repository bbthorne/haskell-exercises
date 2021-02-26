module Golf (skips, localMaxima, histogram) where

import Data.List

-- skips takes a list and returns a list of lists, where each element is every
-- nth element of the original list
skips :: [a] -> [[a]]
skips xs = map (each xs) [1..length xs]
  where each xs n = case drop (n-1) xs of
                      (y:ys) -> y : each ys n
                      []     -> []

-- localMaxima takes a list of integers and returns a list of all local maxima,
-- elements that are greater than both of their immediate neighbors.
localMaxima :: [Integer] -> [Integer]
localMaxima = map second . filter localMax . threes
  where threes (x:xs) = case take 3 (x:xs) of
                          []     -> []
                          (y:ys) -> (y:ys) : threes xs
        localMax (x:y:z:[]) = y > x && y > z
        localMax _          = False
        second (x:y:xs) = y

-- histogram takes a list of Integers between 0 and 9 and outputs a vertical
-- histogram showing how many of each number were in the input list as a String.
-- Ex: histogram [3,5] == "   * *    \n==========\n0123456789\n"
--
--     histogram [2,2,1,3,1,2] == "  *       \n **       \n ***      \n==========\n0123456789\n"
-- 1. Sort the list
-- 2. takewhile = 0 thru 9 and create a list of the lengths of the results
-- 3. construct the histogram by iterating thru the list and adding a * for each
--    nonzero value, subtract from list
histogram :: [Integer] -> String
histogram xs = (consHist . freq [0..9] . sort) xs ++ "==========\n0123456789\n"
  where consHist xs
          | sum xs == 0 = ""
          | otherwise   = (consHist . map (natSub 1)) xs ++ (map col xs ++ "\n")

        col x | x > 0     = '*'
              | otherwise = ' '

        natSub n x | x > 0, n > 0 = x - n
                   | otherwise    = x

-- freq takes 2 lists of integers, and returns a list of frequencies of values
-- in the first list in the second list. It returns a list of Ints.
freq :: [Integer] -> [Integer] -> [Int]
freq []     _  = []
freq (x:xs) ys = let count = (length . takeWhile (== x)) ys
                  in count : freq xs (drop count ys)
