module Party where
import Employee
import Data.Tree
import Data.List
import System.Environment

glCons :: Employee -> GuestList -> GuestList
glCons g (GL gs f) = GL (g:gs) (f + empFun g)

glAppend :: GuestList -> GuestList -> GuestList
glAppend (GL g1s f1) (GL g2s f2) = GL (g1s ++ g2s) (f1 + f2)

instance Semigroup GuestList where
  (<>) = glAppend

instance Monoid GuestList where
  mempty  = GL [] 0
  mappend = glAppend

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFoldl :: (b -> Tree a -> b) -> b -> Tree a -> b
treeFoldl f acc t = f (foldl' (treeFoldl f) acc (subForest t)) t

treeFoldr :: (Tree a -> b -> b) -> b -> Tree a -> b
treeFoldr f acc t = f t $ foldr (flip $ treeFoldr f) acc (subForest t)

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f t = f (rootLabel t) $ map (treeFold f) (subForest t)

nextLevel :: Employee -> [(GuestList,GuestList)] -> (GuestList,GuestList)
nextLevel boss res = (withBoss,woutBoss)
  where withBoss = glCons boss $ mconcat (map snd res)
        woutBoss = mconcat $ map (uncurry moreFun) res

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

parseGL :: GuestList -> String
parseGL (GL gs f) = "Total fun: " ++ show f ++ "\n" ++
                    (unlines . sort) (map empName gs)

main :: IO ()
main = getArgs >>= readFile . head
               >>= putStrLn . parseGL . maxFun . read

-- kinda sloppy with do notation:
mainDo :: IO ()
mainDo = do
    args <- getArgs
    file <- readFile . head $ args
    putStrLn . parseGL . maxFun . read $ file
