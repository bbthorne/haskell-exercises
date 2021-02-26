-- To create an executable, ghc expects a module named "Main" with a func main
-- use -o to make an executable ("linking"). Needs SimpleJSON.o in the dir
-- ghc -o simple Main.hs
module Main (main) where

import SimpleJSON

main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])
