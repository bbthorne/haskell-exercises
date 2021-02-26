import Data.Char (toUpper)

square :: [Double] -> [Double]
square = map (\x -> x * x)

upperCase :: String -> String
upperCase = map toUpper

myMap :: (a -> b) -> [a] -> [b]
myMap f []     = []
myMap f (x:xs) = f x : myMap f xs
