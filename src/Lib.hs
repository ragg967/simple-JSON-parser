module Lib
  ( parseJSON,
  )
where

import Data.Aeson

decode :: (FromJSON a) => ByteString -> Maybe a
encode :: (ToJSON a) => a -> ByteString