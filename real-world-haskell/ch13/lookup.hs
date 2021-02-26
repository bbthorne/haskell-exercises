myLookup :: Eq a => a -> [(a,b)] -> Maybe b
myLookup _ [] = Nothing
myLookup key ((currKey, currVal):rest) = if key == currKey
                                         then Just currVal
                                         else myLookup key rest
