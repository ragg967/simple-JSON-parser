module Lib
  ( parseJSON,
    JSONValue (..),
    parseJSONFromFile,
    parseJSONFromString,
  )
where

import Data.Aeson (FromJSON, ToJSON, Value, decode, encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Simple JSON Value type for demonstration
data JSONValue
  = JSONNull
  | JSONBool Bool
  | JSONNumber Double
  | JSONString Text
  | JSONArray [JSONValue]
  | JSONObject [(Text, JSONValue)]
  deriving (Show, Eq)

-- Main parsing function that demonstrates the library
parseJSON :: IO ()
parseJSON = do
  putStrLn "Simple JSON Parser Demo"
  putStrLn "======================"

  -- Example 1: Parse a simple JSON string
  let jsonString = "{\"name\": \"John\", \"age\": 30, \"active\": true}"
  putStrLn $ "Parsing: " ++ jsonString

  case parseJSONFromString jsonString of
    Just result -> putStrLn $ "Success: " ++ show result
    Nothing -> putStrLn "Failed to parse JSON"

  putStrLn ""

  -- Example 2: Parse an array
  let jsonArray = "[1, 2, 3, \"hello\", true, null]"
  putStrLn $ "Parsing: " ++ jsonArray

  case parseJSONFromString jsonArray of
    Just result -> putStrLn $ "Success: " ++ show result
    Nothing -> putStrLn "Failed to parse JSON"

-- Parse JSON from a string
parseJSONFromString :: String -> Maybe Value
parseJSONFromString s = decode (BLC.pack s)

-- Parse JSON from a file
parseJSONFromFile :: FilePath -> IO (Maybe Value)
parseJSONFromFile filepath = do
  content <- BL.readFile filepath
  return $ decode content

-- Helper function to encode JSON
encodeJSON :: (ToJSON a) => a -> BL.ByteString
encodeJSON = encode

-- Helper function to decode JSON
decodeJSON :: (FromJSON a) => BL.ByteString -> Maybe a
decodeJSON = decode