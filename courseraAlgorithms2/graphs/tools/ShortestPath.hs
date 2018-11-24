import JSON (fromFile, toGraph)
import qualified Options.Applicative.Builder as Builder
import Options.Applicative.Types (Parser)
import Options.Applicative (execParser, helper, (<**>))

newtype Config = Config FilePath

configParser :: Parser Config
configParser =
    Config <$>
    Builder.argument Builder.str
        (Builder.metavar "FILE" <>
         Builder.help "File with map defined in JSON")

main :: IO ()
main = run =<< execParser opts
  where
    opts = Builder.info (configParser <**> helper)
      (Builder.fullDesc
      <> Builder.progDesc "Finds shortest path"
      <> Builder.header "shortestpath - a tool which finds the shortest path in a weighted undirected graph")


run :: Config -> IO ()
run (Config fp) = do
  json <- fromFile fp
  case json of
    Left err -> print err
    Right m -> do
      print $ toGraph m
