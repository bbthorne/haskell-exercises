-- Simple command-line framework

import System.Environment (getArgs)

-- interactWith reads the contents of inputFile and applies function to the
-- contents, writing the result to outputFile.
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = do
  args <- getArgs
  case args of
    [input, output] -> interactWith myFunction input output
    _               -> putStrLn "error: exactly 2 arguments needed"

  -- TODO: replace with the function we'd like to test
  where myFunction = id
