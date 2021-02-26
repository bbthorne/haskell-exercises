import Data.List
import System.IO
import Control.Monad(when)
import System.Exit
import System.Environment(getArgs)

main = do
    -- load the command-line args
    args <- getArgs

    -- error when we don't have the right # of args
    when (length args /= 2) $ do
        putStrLn "Syntax: passwd-al filename uid"
        exitFailure

    -- read the file lazily
    content <- readFile (args !! 0)

    -- compute the username (pure code)
    let username = findByUID content (read (args !! 1))

    -- display result
    case username of
        Just x  -> putStrLn x
        Nothing -> putStrLn "Could not find that UID"

-- given the entire input and the UID, look for a username
findByUID :: String -> Integer -> Maybe String
findByUID content uid = let al = map parseline . lines $ content
                        in lookup uid al

-- convert a colon-separated line into fields
parseline :: String -> (Integer, String)
parseline input = let fields = split ':' input
                  in (read (fields !! 2), fields !! 0)

-- takes a delimiter and a list, breaks up the list by delimiter
split :: Eq a => a -> [a] -> [[a]]
split _     []  = [[]]
split delim str = let (before, remainder) = span (/= delim) str
                  in before : case remainder of
                                  [] -> []
                                  x  -> split delim (tail x)
