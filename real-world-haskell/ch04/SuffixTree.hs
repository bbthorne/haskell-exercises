-- Can use @ to bind a variable on the left to a value on the right in a pattern
-- Called an "as-pattern"

suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs : suffixes xs'
suffixes _          = []

-- can also use init composed with tails 
suffixes2 = init . tails
