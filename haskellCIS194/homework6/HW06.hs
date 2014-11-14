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

-- | Exercise 1 - convert "Y" to Bool
-- >>> ynToBool (String "Y")
-- Bool True
-- >>> ynToBool (Object (fromList [("country",String "Alameda"), ("credits",String "Y")]))
-- Object (fromList [("country",String "Alameda"),("credits",Bool True)])
-- >>> ynToBool (String "N")
-- Bool False
--
ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool (String x) = String x
ynToBool (Object o) = Object (fmap (ynToBool) o)


-- | Exercise 2 - returns either an error or a Value
-- >>> parseData (B.pack "")
-- Left "not enough input"
-- >>> parseData (B.pack "{\"credit\":\"Y\"}")
-- Right (Bool False)
parseData :: B.ByteString -> Either String Value
parseData xs = fmap ynToBool $ eitherDecode xs
