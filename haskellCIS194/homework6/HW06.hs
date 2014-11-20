{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module HW06 where

import Data.Aeson
import Data.Monoid
import GHC.Generics
import Data.List

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
ynToBool (Object x) = Object (fmap (ynToBool) x)
ynToBool (Array x) = Array (fmap ynToBool x)
ynToBool x = x


-- | Exercise 2 - returns either an error or a Value
-- >>> parseData (B.pack "")
-- Left "not enough input"
-- >>> parseData (B.pack "{\"credit\":\"Y\"}")
-- Right (Object (fromList [("credit",Bool True)]))
parseData :: B.ByteString -> Either String Value
parseData xs = fmap ynToBool $ eitherDecode xs

-- | Exercise 3 - market type
data Market = Market { marketname :: T.Text
                     , x          :: Int
                     , y          :: Int
                     , wine       :: Bool
                     , state      :: String }
  deriving (Show, Generic)

instance FromJSON Market


-- | Exercise 3 - parse JSON to Market data types
parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets = eitherDecode


-- | Exercise 4 - loads market data
loadData :: IO [Market]
loadData = do
    filedata <- B.readFile "markets.json"
    case parseMarkets filedata of
        Left err -> fail err
        Right xs -> return $ xs


-- | Exercise 5
-- >>> combined
-- OrdList {getOrdList = [1,2,3,4,5,6]}
data OrdList a = OrdList { getOrdList :: [a] }
    deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
    mempty  = OrdList ([])
    mappend (OrdList xs) (OrdList ys) = OrdList $ sort $ xs <> ys

evens :: OrdList Integer
evens = OrdList [2,4,6]

odds :: OrdList Integer
odds = OrdList [1,3,5]

combined :: OrdList Integer
combined = evens <> odds
