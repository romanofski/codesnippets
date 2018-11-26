module Main where

import Test.DocTest

main :: IO ()
main =
    doctest
        [ "-isrc"
        , "src/Main.hs"
        , "src/Graph.hs"
        , "src/JSON.hs"
        , "src/Dijkstra.hs"]
