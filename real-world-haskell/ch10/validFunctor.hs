data Foo a = Foo a

instance Functor Foo where
    fmap f (Foo a) = Foo (f a)

-- putting a type constraint in a data def = bad. will need to update the types
-- of all functions that act on that value. Instead, add type constraints to
-- functions that act on the type.
-- data Eq a => Bar a = Bar a
--
-- instance Functor Bar where
--     fmap f (Bar a) = Bar (f a)
