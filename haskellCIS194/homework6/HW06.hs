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

-- | Exercise 1 - convert "Y" to Bool
-- >>> ynToBool (String "Y")
-- Bool True
-- >>> ynToBool (String "N")
-- Bool False
--
ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool _            = Bool False
