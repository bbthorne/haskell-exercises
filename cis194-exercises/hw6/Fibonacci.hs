{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}
module Fibonacci where

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = fibo 0 1
  where fibo a b = a : fibo b (a + b)

-- Exercise 3
data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = show . streamToList

-- Exercise 4
streamToList :: Stream a -> [a]
streamToList (Cons v vs) = v : streamToList vs

streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons v vs) = Cons (f v) (streamMap f vs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f v = Cons v (streamFromSeed f $ f v)

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

ruler :: Stream Integer
ruler = undefined

-- Exercise 6
x :: Stream Integer
x = Cons 0 (Cons 1 $ streamRepeat 0)

instance Num (Stream Integer) where
  fromInteger n = Cons n $ streamRepeat 0
  negate = streamMap negate
  (+) (Cons a as) (Cons b bs) = Cons (a + b) (as + bs)
  (*) (Cons a as) (Cons b bs) = Cons (a * b) $ rest
    where rest = streamMap (* a) bs + (Cons b bs * as)

instance Fractional (Stream Integer) where
  (/) (Cons a as) (Cons b bs) = Cons (div a b) rest
    where rest = streamMap (* (div 1 b)) (as - ((Cons a as / Cons b bs) * bs))

fib3 :: Stream Integer
fib3 = x / (1 - x - x^2)
