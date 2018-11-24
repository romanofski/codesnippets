{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module JSON where

import GHC.Generics
import Data.Aeson
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

import Graph (Graph, Vertex, Weight, buildUndirectedGraph)

type Distances = Map.Map Char Int
type Towns = Map.Map Char Distances
newtype JSONMap = JSONMap
    { map :: [Towns]
    } deriving (Generic,Show)

instance FromJSON JSONMap where

fromFile :: FilePath -> IO (Either String JSONMap)
fromFile = eitherDecodeFileStrict

toGraph :: JSONMap -> Graph k a
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
