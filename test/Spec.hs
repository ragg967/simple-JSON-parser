import Test.Hspec
import Lib
import Data.Aeson (Value(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Text (Text)

main :: IO ()
main = hspec $ do
  describe "JSON Parser" $ do
    it "parses simple JSON object" $ do
      let json = "{\"name\": \"John\", \"age\": 30}"
      case parseJSONFromString json of
        Just (Object obj) -> do
          HM.lookup "name" obj `shouldBe` Just (String "John")
          HM.lookup "age" obj `shouldBe` Just (Number 30)
        _ -> expectationFailure "Failed to parse JSON object"
    
    it "parses JSON array" $ do
      let json = "[1, 2, 3]"
      case parseJSONFromString json of
        Just (Array arr) -> do
          V.length arr `shouldBe` 3
          V.toList arr `shouldBe` [Number 1, Number 2, Number 3]
        _ -> expectationFailure "Failed to parse JSON array"
    
    it "parses JSON with different types" $ do
      let json = "{\"str\": \"hello\", \"num\": 42, \"bool\": true, \"null\": null}"
      case parseJSONFromString json of
        Just (Object obj) -> do
          HM.lookup "str" obj `shouldBe` Just (String "hello")
          HM.lookup "num" obj `shouldBe` Just (Number 42)
          HM.lookup "bool" obj `shouldBe` Just (Bool True)
          HM.lookup "null" obj `shouldBe` Just Null
        _ -> expectationFailure "Failed to parse JSON with mixed types"
    
    it "returns Nothing for invalid JSON" $ do
      let json = "{invalid json"
      parseJSONFromString json `shouldBe` Nothing