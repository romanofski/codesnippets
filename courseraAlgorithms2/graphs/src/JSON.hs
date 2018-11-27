{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module JSON (
    fromFile
  , toGraph
  , parseOperations
  , ShortestPath(..)
  , JSONMap(..)
  , GraphOperation(..)
  , shortestPathToType
  , MapOperation(..)
  -- re-exports from Data.Aeson
  , encode
  , decodeString
  ) where

import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Control.Lens (both, firstOf)
import Control.Applicative ((<|>))
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as Char8

import Graph (Graph, Vertex, Weight, buildUndirectedGraph, Edge(..))

type Distances = Map.Map Char Int
type Towns = Map.Map Char Distances
newtype JSONMap = JSONMap
    { map :: [Towns]
    } deriving (Generic,Show)

instance FromJSON JSONMap where

parseGraph :: Value -> Parser Graph
parseGraph v = do
  p <- pure v >>= parseJSON :: Parser (Maybe JSONMap)
  case p of
    Just f -> pure (toGraph f)
    Nothing -> fail "Can not parse JSON to build graph"

fromFile :: FilePath -> IO (Either String JSONMap)
fromFile = eitherDecodeFileStrict

-- | Convenience method to decode a string
-- Note: Only used to make optparse happy and for the purpose of this homework.
-- For production usage, use ByteString and Data.Aeson.decode
decodeString :: FromJSON a => String -> Either String a
decodeString = eitherDecodeStrict . Char8.pack

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

{-
-- Operations which can be applied on the map
-}

-- | A high level map operation for querying producing a result
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
  parseJSON = parseMapOps

parseMapOps :: Value -> Parser MapOperation
parseMapOps = parseQuery

parseQuery :: Value -> Parser MapOperation
parseQuery (Object v) = do
    x <- v .: "start"
    y <- v .: "end"
    case both translateTownNameToVertex (x,y) of
      Just (x', y') -> QueryDistance <$> pure x' <*> pure y'
      Nothing -> fail "Unable to construct operation instance from JSON"
parseQuery _ = mempty

-- | Result type to JSON encode shortest distance calculated
newtype ShortestPath = ShortestPath { distance :: Integer }
  deriving (Show, Generic)

-- |
-- >>> encode (ShortestPath 10)
-- "{\"distance\":10}"
instance ToJSON ShortestPath where
  toEncoding = genericToEncoding defaultOptions


-- | convenience function to wrap the shortest path result in a type for
-- encoding it to JSON
shortestPathToType :: Maybe Double -> Maybe ShortestPath
shortestPathToType = fmap (ShortestPath . round)


-- | operation defined which is only applied to the graph itself
data GraphOperation
    = Add Graph
    | ReplaceEdge Edge
    deriving (Show)

instance FromJSON GraphOperation where
  parseJSON v = parseAdd v <|> parseReplaceEdge v

-- | Add operation to add nodes to the graph
-- >>> :set -XOverloadedStrings
-- >>> decode "{ \"map\": [{ \"A\": {\"I\":70, \"J\":150} }]}" :: Maybe GraphOperation
-- Just (Add (fromList [(0,[Edge (0,8) 70.0,Edge (0,9) 150.0]),(8,[Edge (8,0) 70.0]),(9,[Edge (9,0) 150.0])]))
parseAdd :: Value -> Parser GraphOperation
parseAdd v = parseGraph v >>= pure . Add

-- | Replace edge operation
-- >>> :set -XOverloadedStrings
-- >>> decode "{ \"A\": {\"B\": 80}}" :: Maybe GraphOperation
-- Just (ReplaceEdge (Edge (0,1) 80.0))
parseReplaceEdge :: Value -> Parser GraphOperation
parseReplaceEdge v = do
  town <- pure v >>= parseJSON :: Parser (Maybe Towns)
  case town of
    Just t -> case firstOf traverse (buildEdgeToNodes t) of
      Just (x, y, w) -> pure $ ReplaceEdge (Edge (x, y) w)
      Nothing -> fail "Unable to construct operation to replace roads. Failed to map characters."
    Nothing -> fail "Unable to construct operation to replace roads"
