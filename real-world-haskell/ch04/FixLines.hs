-- A program that converts newlines to be OS appropriate

import System.Environment (getArgs)

-- interactWith reads the contents of inputFile and applies function to the
-- contents, writing the result to outputFile.
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

-- splitlines splits a file in a string into a list of each line as a string.
-- it uses Python's "universal newline" as inspiration.
-- break is predefined. takes a predicate and a list, returning a pair with 2
-- lists representing the elements before the predicate returned True and after.
splitlines :: String -> [String]
splitlines [] = []
splitlines cs = let (pre, suf) = break isLineTerminator cs
                in pre : case suf of
                           ('\r':'\n':rest) -> splitlines rest
                           ('\r':rest)      -> splitlines rest
                           ('\n':rest)      -> splitlines rest
                           _                -> []

isLineTerminator c = c == '\r' || c == '\n'

fixlines :: String -> String
fixlines input = unlines (splitlines input)

main = do
  args <- getArgs
  case args of
    [input, output] -> interactWith myFunction input output
    _               -> putStrLn "error: exactly 2 arguments needed"

  -- TODO: replace with the function we'd like to test
  where myFunction = fixlines
