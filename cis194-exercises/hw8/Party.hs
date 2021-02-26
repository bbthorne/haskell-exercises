module Party where
import Employee
import Data.Tree
import Data.List

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + empFun e)

glAppend :: GuestList -> GuestList -> GuestList
glAppend gl (GL (e:es) f) = glAppend (glCons e gl) (GL es f)
glAppend gl (GL [] _)     = gl

instance Semigroup GuestList where
  (<>) = glAppend

instance Monoid GuestList where
  mempty  = GL [] 0
  mappend = glAppend

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2
  | gl1 > gl2 = gl1
  | otherwise = gl2

treeFoldl :: (b -> Tree a -> b) -> b -> Tree a -> b
treeFoldl f acc t = f (foldl' (treeFoldl f) acc (subForest t)) t

treeFoldr :: (Tree a -> b -> b) -> b -> Tree a -> b
treeFoldr f acc t = f t $ foldr (flip $ treeFoldr f) acc (subForest t)

-- I need to re-think this using the actual fun scores of each GuestList
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList,GuestList)
nextLevel bob ((wBoss,noBoss):gls) = foldr combine (glCons bob noBoss,wBoss) gls
  where combine (wE,nE) (wBob,noBob) = (glAppend nE wBob, glAppend wE noBob)

maxFun :: Tree Employee -> GuestList
maxFun
