module JoinList where
import Sized
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ x     = x
x     +++ Empty = x
x     +++ y     = Append (tag x <> tag y) x y

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

subtractSize :: (Sized b, Monoid b) => Int -> JoinList b a -> Int
subtractSize i jl = i - (getSize . size . tag) jl

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty              = Nothing
indexJ 0 (Single _ v)       = Just v
indexJ _ (Single _ v)       = Nothing
indexJ i (Append _ jl1 jl2)
  | newI >= 0               = indexJ newI jl2
  | otherwise               = indexJ i jl1
  where newI = subtractSize i jl1

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty              = Empty
dropJ i jl | i <= 0        = jl
dropJ _ (Single _ _)       = Empty
dropJ i (Append m jl1 jl2)
  | newI >= 0              = dropJ newI jl2
  | otherwise              = dropJ i jl1 +++ jl2
  where newI = subtractSize i jl1

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty                 = Empty
takeJ i jl | i <= 0           = Empty
takeJ _ jl@(Single _ _)       = jl
takeJ i jl@(Append m jl1 jl2)
  | newI >= 0                 = jl1 +++ takeJ newI jl2
  | otherwise                 = takeJ i jl1
  where newI = subtractSize i jl1

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s
