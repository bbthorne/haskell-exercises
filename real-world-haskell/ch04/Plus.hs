-- Can define or apply a function of 2 arguments with infix notation using '``'
-- Prefix notation more common but infix can oftentimes be more readable 
a `plus` b = a + b

data a `Pair` b = a `Pair` b
                  deriving (Show)

foo = Pair 1 2
bar = True `Pair` "quux"
