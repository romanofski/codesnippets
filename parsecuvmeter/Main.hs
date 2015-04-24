-- very simple XML parser designed to parse UV data XML
--
import Text.Parsec
import XMLParser

-- | finds uvrating
--
-- >>> let xml = [Decl " version=\"1.0\" encoding=\"UTF-8\"",Element "stations" [] [Element "location" [Attribute ("id","adelaide")] [Element "name" [] [Body "adl"],Element "index" [] [Body "0.1"]]]]
-- >>> findRating "adelaide" xml
-- "0.1"
--
findRating :: String -> [XML] ->  String
findRating locID (Element "stations" _ y:_) = findRating locID y
findRating locID (Element "location" [Attribute attr] ys:xs)
    | locID == snd attr = findRating locID ys
    | otherwise = findRating locID xs
findRating _ (Element "index" [] [Body rate]:_) = rate
findRating locID (_:xs) = findRating locID xs
findRating _ [] = ""

main :: IO ()
main = do
    contents <- getContents
    case (parse document "" contents) of
        Right xml -> putStrLn $ findRating "brisbane" xml
        Left  err -> putStrLn $ show err
