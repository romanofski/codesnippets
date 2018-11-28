-- very simple XML parser designed to parse UV data XML
--
import Text.Parsec
import XMLParser
import Data.Maybe
import qualified Control.Exception as CE
import Text.Read (readMaybe)
import Data.ByteString.Lazy.Char8 as B
import Network.HTTP.Conduit
       (parseRequest, newManager, tlsManagerSettings, httpLbs,
        responseBody)

uvURL :: String
uvURL = "https://uvdata.arpansa.gov.au/xml/uvvalues.xml"

getData :: IO String
getData =
  CE.catch (do request <- parseRequest uvURL
               manager <- newManager tlsManagerSettings
               res <- httpLbs request manager
               return $ B.unpack $ responseBody res)
           errHandler
  where errHandler
          :: CE.SomeException -> IO String
        errHandler _ = return "<Could not retrieve data>"

textToXMLDocument :: String -> Either ParseError [XML]
textToXMLDocument = parse document ""

getUVRating :: String -> [XML] ->  Maybe Float
getUVRating locID (Element "stations" _ y:_) = getUVRating locID y
getUVRating locID (Element "location" [Attribute attr] ys:xs)
    | locID == snd attr = getUVRating locID ys
    | otherwise = getUVRating locID xs
getUVRating _ (Element "index" [] [Body rate]:_) = readMaybe rate
getUVRating locID (_:xs) = getUVRating locID xs
getUVRating _ [] = Nothing

main :: IO ()
main = do
    contents <- getData
    case (parse document "" contents) of
        Right xml -> print $ getUVRating "Brisbane" xml
        Left  err -> print $ show err
