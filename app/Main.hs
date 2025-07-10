module Main (main) where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      -- Run the demo if no arguments provided
      parseJSON
    [filepath] -> do
      -- Parse JSON from file if filepath is provided
      putStrLn $ "Parsing JSON from file: " ++ filepath
      result <- parseJSONFromFile filepath
      case result of
        Just json -> putStrLn $ "Success: " ++ show json
        Nothing -> putStrLn "Failed to parse JSON from file"
    _ -> do
      putStrLn "Usage: simple-JSON-parser-exe [filepath]"
      putStrLn "  Run without arguments for demo"
      putStrLn "  Provide filepath to parse JSON from file"