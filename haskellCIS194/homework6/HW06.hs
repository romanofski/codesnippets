{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module HW06 where

import Data.Aeson
import Data.Monoid
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T

-- $setup
-- The code need the following language pragmas
-- >>> :set -XOverloadedStrings
-- >>> :set -XDeriveGeneric
-- >>> import Data.HashMap.Strict

toValue :: Maybe Value -> Value
toValue (Just a) = a
toValue (Nothing) = Null

-- | Exercise 1 - convert "Y" to Bool
-- >>> ynToBool (String "Y")
-- Bool True
-- >>> ynToBool $ toValue $ (decode "{\"country\" : \"Alameda\", \"credits\" : \"Y\" }" :: Maybe Value)
-- Object (fromList [("country",String "Alameda"),("credits",Bool True)])
-- >>> ynToBool $ toValue $ (decode (encode [1,3,4]) :: Maybe Value)
-- Array (fromList [Number 1.0,Number 3.0,Number 4.0])
-- >>> ynToBool (String "N")
-- Bool False
--
ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool (String x) = String x
ynToBool (Object x) = Object (fmap (ynToBool) x)
ynToBool (Array x) = Array (fmap ynToBool x)
ynToBool (Number x) = Number x
ynToBool _ = Bool False


-- | Exercise 2 - returns either an error or a Value
-- >>> parseData (B.pack "")
-- Left "not enough input"
-- >>> parseData (B.pack "{\"credit\":\"Y\"}")
-- Right (Object (fromList [("credit",Bool False)]))
parseData :: B.ByteString -> Either String Value
parseData xs = eitherDecode xs

-- | Exercise 3 - market type
--
data Market = Market { marketname :: T.Text
                     , x          :: Int
                     , y          :: Int
                     , state      :: String }
  deriving (Show, Generic)
instance FromJSON Market


parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets xs = eitherDecode xs
