import qualified Data.Map as Map

-- Ways to make a map: from association list or from scratch
al = [(1, "one"), (2, "two"), (3, "three"), (4, "four")]

mapFromAL = Map.fromList al

mapFold = foldl (\map (k,v) -> Map.insert k v map) Map.empty al

-- from scratch: maps do not guaruntee ordering, only (k,v)
mapManual = Map.insert 2 "two" .
            Map.insert 4 "four" .
            Map.insert 1 "one" .
            Map.insert 3 "three" $ Map.empty
