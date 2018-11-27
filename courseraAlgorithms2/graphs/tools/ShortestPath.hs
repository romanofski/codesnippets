import qualified Options.Applicative.Builder as Builder
import Options.Applicative.Types (Parser, ReadM(..))
import Options.Applicative (execParser, helper, (<**>))
import Options.Applicative.Help.Pretty (text, Doc, vcat, (</>), linebreak)
import Control.Applicative (many)

import JSON
       (fromFile, toGraph, parseOperations, MapOperation(..),
        shortestPathToType, GraphOperation(..),
        decodeString, encode)
import Graph (Graph, replaceUndirectedEdge)
import Dijkstra (getShortestPath)

-- |
-- TODO: this uses `String`, but we would prefer ByteString or Text
mapOperation :: ReadM MapOperation
mapOperation = Builder.eitherReader parseOperations

graphOperation :: ReadM GraphOperation
graphOperation = Builder.eitherReader decodeString

data Config = Config FilePath MapOperation [GraphOperation]

configParser :: Parser Config
configParser =
    Config <$>
    Builder.argument
        Builder.str
        (Builder.metavar "FILE" <> Builder.help "File with map defined in JSON") <*>
    Builder.argument
        mapOperation
        (Builder.metavar "QUERY" <> Builder.help "Query the shortest path") <*>
    many
        (Builder.argument
             graphOperation
             (Builder.metavar "OPS" <>
              Builder.help "Add/Replace points and routes"))

verboseHelp :: Doc
verboseHelp =
    text "Example" </> linebreak </>
    vcat
        (text <$>
         [ "Use the map.json for initialisation, calculate shortest path"
         , "from A to J, add additional points I and J from point F and reduce the"
         , "route weight from A to B to 20"]) </>
    linebreak </>
    text
        "stack exec shortestpath tests/data/map.json '{\"start\":\"A\", \"end\":\"J\" }' '{ \"map\": [{ \"F\": {\"I\":70, \"J\":150} }]}' '{ \"A\": {\"B\":20} }'"

main :: IO ()
main = run =<< execParser opts
  where
    opts = Builder.info (configParser <**> helper)
      (Builder.fullDesc
      <> Builder.progDesc "Finds shortest path"
      <> Builder.progDescDoc (Just verboseHelp)
      <> Builder.header "shortestpath - a tool which finds the shortest path in a weighted undirected graph")

applyGraphOperation :: Graph -> GraphOperation -> Graph
applyGraphOperation g (Add g') = g' <> g
applyGraphOperation g (ReplaceEdge e) = replaceUndirectedEdge e g

run :: Config -> IO ()
run (Config fp query ops) = do
  json <- fromFile fp
  case json of
    Left err -> print err
    Right m -> do
      let g = foldr (flip applyGraphOperation) (toGraph m) ops
      print (go query g)
  where
    go (QueryDistance s d) = encode . shortestPathToType . getShortestPath s d
