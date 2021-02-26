module Party where
import Employee
import Data.Tree
import Data.List

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + empFun e)

glAppend :: GuestList -> GuestList -> GuestList
glAppend (GL g1s f1) (GL g2s f2) = GL (g1s ++ g2s) (f1 + f2)

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

pairMap :: (a -> b) -> (a,a) -> (b,b)
pairMap f (x,y) = (f x, f y)

nextLevel :: Employee -> [(GuestList,GuestList)] -> (GuestList,GuestList)
nextLevel boss = formGLs . pairMap mconcat . unzip
  where formGLs (bosses,noboss) = (glCons boss noboss, moreFun bosses noboss)

maxFun :: Tree Employee -> GuestList
maxFun = undefined
