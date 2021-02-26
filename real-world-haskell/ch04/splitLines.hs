-- splitlines splits a file in a string into a list of each line as a string.
-- it uses Python's "universal newline" as inspiration.
-- break is predefined. takes a predicate and a list, returning a pair with 2
-- lists representing the elements before the predicate returned True and after.
splitLines :: String -> [String]
splitlines [] = []
splitlines cs = let (pre, suf) = break isLineTerminator cs
                in pre : case suf of
                           ('\r':'\n':rest) -> splitlines rest
                           ('\r':rest)      -> splitlines rest
                           ('\n':rest)      -> splitlines rest
                           _                -> []

isLineTerminator c = c == '\r' || c == '\n'
