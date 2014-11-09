module Main where

import HW06
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do
    filedata <- B.readFile "markets.json"
    parseData filedata
    putStrLn "Done"
