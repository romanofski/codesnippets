import qualified Options.Applicative.Builder as Builder
import Options.Applicative.Types (Parser, ReadM(..))
import Options.Applicative (execParser, helper, (<**>))

import JSON (fromFile, toGraph, parseOperations, MapOperation(..))
import Dijkstra (getShortestPath)

-- |
-- TODO: this uses `String`, but we would prefer ByteString or Text
mapOperation :: ReadM MapOperation
mapOperation = Builder.eitherReader parseOperations

data Config = Config FilePath MapOperation

configParser :: Parser Config
configParser =
    Config <$>
    Builder.argument Builder.str
        (Builder.metavar "FILE" <>
         Builder.help "File with map defined in JSON")
    <*> Builder.argument mapOperation (Builder.metavar "OPS" <> Builder.help "Operations in JSON")

main :: IO ()
main = run =<< execParser opts
  where
    opts = Builder.info (configParser <**> helper)
      (Builder.fullDesc
      <> Builder.progDesc "Finds shortest path"
      <> Builder.header "shortestpath - a tool which finds the shortest path in a weighted undirected graph")


run :: Config -> IO ()
run (Config fp (QueryDistance s d)) = do
  json <- fromFile fp
  case json of
    Left err -> print err
    Right m -> print $ getShortestPath s d (toGraph m)
