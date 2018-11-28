
module XMLParser where

-- very simple XML parser designed to parse UV data XML
--
-- Kudos to:
-- http://charlieharvey.org.uk/page/naive_xml_parser_with_haskell_parsec_and_perl_regexen_part_one_haskell/
-- for the great article to get started.
--
import Control.Applicative hiding ((<|>),many)
import Text.Parsec
import Text.Parsec.String
import Control.Monad (void)

type AttrName  = String
type AttrValue = String

data Attribute = Attribute (AttrName, AttrValue)
    deriving (Show)

data XML = Element String [Attribute] [XML]
         | Decl String
         | Body String
    deriving (Show)

-- | parse the document
-- Note: I'm ignoring the XML declaration
--
document :: Parser [XML]
document = do
    spaces
    y <- try xmlDecl <|> tag
    spaces
    x <- many tag
    spaces
    return (y : x)

-- | parse any tags
--
tag :: Parser XML
tag  = do
    char '<'
    spaces
    name <- many (letter <|> digit)
    spaces
    attr <- many attribute
    spaces
    string ">"
    eBody <- many elementBody
    endTag name
    spaces
    return (Element name attr eBody)

xmlDecl :: Parser XML
xmlDecl = do
    void $ manyTill anyToken (string "<?xml") -- ignore the byte order mark
    decl <- many (noneOf "?>")
    string "?>"
    return (Decl decl)

elementBody :: Parser XML
elementBody = spaces *> try tag <|> text

endTag :: String -> Parser String
endTag str = string "</" *> string str <* char '>'

text :: Parser XML
text = Body <$> many1 (noneOf "><")

attribute :: Parser Attribute
attribute = do
    name <- many (noneOf "= />")
    spaces
    char '='
    spaces
    char '"'
    value <- many (noneOf "\"")
    char '"'
    spaces
    return (Attribute (name, value))
