{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module JSON (
    fromFile
  , toGraph
  , parseOperations
  , ShortestPath(..)
  , shortestPathToType
  , MapOperation(..)) where

import GHC.Generics (Generic)
import Data.Aeson
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Control.Lens (both)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as Char8

import Graph (Graph, Vertex, Weight, buildUndirectedGraph)

type Distances = Map.Map Char Int
type Towns = Map.Map Char Distances
newtype JSONMap = JSONMap
    { map :: [Towns]
    } deriving (Generic,Show)

instance FromJSON JSONMap where

fromFile :: FilePath -> IO (Either String JSONMap)
fromFile = eitherDecodeFileStrict

toGraph :: JSONMap -> Graph
toGraph (JSONMap m) = buildUndirectedGraph $ foldr (\x a -> a <> buildEdgeToNodes x) [] m

-- |
-- >>> buildEdgeToNodes (Map.fromList [('A', Map.fromList [('B', 100), ('C', 30)])])
-- [(0,2,30.0),(0,1,100.0)]
buildEdgeToNodes :: Towns -> [(Vertex, Vertex, Weight)]
buildEdgeToNodes = Map.foldrWithKey (\k edges a -> a <> edgeToNode k edges) []

-- |
-- TODO: No error handling :( This includes `translateTownNameToVertex`
-- >>> edgeToNode 'A' (Map.fromList [('A', 30), ('B', 40)])
-- [(0,1,40.0),(0,0,30.0)]
edgeToNode :: Char -> Distances -> [(Vertex, Vertex, Weight)]
edgeToNode k e =
    let v = fromMaybe 0 . translateTownNameToVertex
    in Map.foldrWithKey
           (\town weight acc -> acc <> [(v k, v town, fromIntegral weight)]) [] e

-- |
-- >>> translateTownNameToVertex 'A'
-- Just 0
-- >>> translateTownNameToVertex 'F'
-- Just 5
-- >>> translateTownNameToVertex 'a'
-- Just 26
translateTownNameToVertex :: Char -> Maybe Vertex
translateTownNameToVertex = flip elemIndex (['A'..'Z'] <> ['a'..'z'])


-- | A map operation is used to query or add new vertices to the graph
data MapOperation =
    QueryDistance Vertex
                  Vertex
    deriving (Show)

parseOperations :: String -> Either String MapOperation
parseOperations = eitherDecodeStrict . Char8.pack

-- |
-- >>> :set -XOverloadedStrings
-- >>> decode "{\"start\": \"A\", \"end\": \"B\"}" :: Maybe MapOperation
-- Just (QueryDistance 0 1)
-- >>> decode "{\"end\": [\"B\", \"C\"]}" :: Maybe MapOperation
-- Nothing
-- >>> decode "[\"A\", \"B\", \"C\"]" :: Maybe MapOperation
-- Nothing
instance FromJSON MapOperation where
  parseJSON (Object v) = do
    x <- v .: "start"
    y <- v .: "end"
    case both translateTownNameToVertex (x,y) of
      Just (x', y') -> QueryDistance <$> pure x' <*> pure y'
      Nothing -> fail "Unable to construct operation instance from JSON"
  parseJSON _ = mempty

-- | Result type to JSON encode shortest distance calculated
newtype ShortestPath = ShortestPath { distance :: Integer }
  deriving (Show, Generic)

-- |
-- >>> encode (ShortestPath 10)
-- "{\"distance\":10}"
instance ToJSON ShortestPath where
  toEncoding = genericToEncoding defaultOptions

-- | convenience function to wrap the shortest path result in a type for encoding it to JSON
shortestPathToType :: Maybe Double -> Maybe ShortestPath
shortestPathToType = fmap (ShortestPath . round)
