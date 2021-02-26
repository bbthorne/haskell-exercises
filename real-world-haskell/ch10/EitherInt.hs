{-# LANGUAGE FlexibleInstances #-}

instance Functor (Either Int) where
    fmap _ (Left n) = Left n
    fmap f (Right r) = Right (f r)

-- 2 rules for functors:
-- 1. preserve identity: fmap id is a function that returns the argument
-- 2. composable: fmap f . fmap g == fmap (f . g)
-- Functors preserve shape: then structure is never affected, just values
