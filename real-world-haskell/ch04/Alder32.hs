import Data.Char (ord)

-- shiftL = shift left, (.&.) = bitwise and, (.|.) = bitwise or
import Data.Bits (shiftL, (.&.), (.|.))

alder32 = helper (1,0)
    where base                = 65521
          helper (a,b) (x:xs) =
              let a' = (a + (ord x .&. 0xff)) `mod` base
                  b' = (a' + b) `mod` base
              in helper (a', b') xs
          helper (a,b) _      = (b `shiftL` 16) .|. a

-- use foldl instead!
alder32_foldl xs = let (a,b) = foldl step (1,0) xs
                   in (b `shiftL` 16) .|. a
    where step (a, b) x = let a' = (a + (ord x .&. 0xff)) `mod` base
                              b' = (a' + b) `mod` base
                          in (a', b')

-- foldr can be useful sometimes
-- it can be used instead of append
myAppend :: [a] -> [a]
myAppend xs ys = foldr (:) ys xs

-- since thunks are big, foldl kinda sucks and can fail on large lists
-- but the Data.List module defines foldl' which doesn't build up thunks
