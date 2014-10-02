{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import qualified Data.Char
import qualified Data.List

-- | Helper function for Exercise 1
-- >>> toLogMessage ["I", "29", "la"]
-- ValidLM (LogMessage Info 29 "la")
-- >>> toLogMessage ["E", "2", "29", "help"]
-- ValidLM (LogMessage (Error 2) 29 "help")
-- >>> toLogMessage ["Foo", "bar", "baz"]
-- InvalidLM "Foo bar baz"
toLogMessage :: [String] -> MaybeLogMessage
toLogMessage ("I":y:xs) = ValidLM (LogMessage Info (read y :: TimeStamp) (unwords xs))
toLogMessage ("W":y:xs) = ValidLM (LogMessage Warning (read y :: TimeStamp) (unwords xs))
toLogMessage ("E":x:y:xs) = ValidLM (LogMessage (Error (read x)) (read y) (unwords xs))
toLogMessage p = InvalidLM (unwords p)

-- | Exercise 1
-- >>> parseMessage "E 2 562 help help"
-- ValidLM (LogMessage (Error 2) 562 "help help")
-- >>> parseMessage "I 29 la la la"
-- ValidLM (LogMessage Info 29 "la la la")
-- >>> parseMessage "Not the right format"
-- InvalidLM "Not the right format"
parseMessage :: String -> MaybeLogMessage
parseMessage s = toLogMessage (words s)


-- | Helper function
-- >>> isValidLM (ValidLM (LogMessage (Error 2) 562 "hp"))
-- True
-- >>> isValidLM (parseMessage "not the right")
-- False
isValidLM :: MaybeLogMessage -> Bool
isValidLM (ValidLM _) = True
isValidLM _ = False

-- | Exercise 2
-- >>> validMessagesOnly [ValidLM (LogMessage (Error 2) 562 "hp"), InvalidLM "foo"]
-- [LogMessage (Error 2) 562 "hp"]
validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly [] = []
validMessagesOnly (ValidLM lm:xs) = lm:validMessagesOnly xs
validMessagesOnly (_:xs) = validMessagesOnly xs

-- | Exercise 3
parse :: String -> [LogMessage]
parse x = validMessagesOnly $ map parseMessage (lines x)

-- | Exercise 4
-- >>> compareMsgs (LogMessage Warning 153 "Foo") (LogMessage Info 208 "baz")
-- LT
-- >>> compareMsgs (LogMessage (Error 101) 22 "My God") (LogMessage Info 22 "My God")
-- EQ
compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ x _) (LogMessage _ y _)
    | x < y = LT
    | x > y = GT
    | otherwise = EQ


-- | Exercise 5
sortMessages :: [LogMessage] -> [LogMessage]
sortMessages = Data.List.sortBy compareMsgs

-- | Helper
-- >>> isImportantMsg (LogMessage (Error 101) 22 "My God")
-- True
-- >>> isImportantMsg (LogMessage Info 22 "My God")
-- False
isImportantMsg :: LogMessage -> Bool
isImportantMsg (LogMessage (Error x) _ _)
    | x >= 50 = True
    | otherwise = False
isImportantMsg LogMessage{} = False


logMessageString :: LogMessage -> String
logMessageString (LogMessage _ _ x) = x

-- | Exercise 6
-- >>> whatWentWrong [(LogMessage (Error 1) 51 "Foo"),(LogMessage (Error 208) 51 "baz")]
-- ["baz"]
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = map logMessageString (filter isImportantMsg xs)


isInfixOfMessage :: String -> LogMessage -> Bool
isInfixOfMessage x (LogMessage _ _ y) = l `Data.List.isInfixOf` m
    where l = map Data.Char.toLower x
          m = map Data.Char.toLower y

-- | Exercise 7
-- >>> let xs = [(LogMessage (Error 102) 51 "Foo init"),(LogMessage (Error 208) 51 "baz")]
-- >>> messagesAbout "Init" xs
-- [LogMessage (Error 102) 51 "Foo init"]
messagesAbout :: String -> [LogMessage] -> [LogMessage]
messagesAbout s = filter (isInfixOfMessage s)

-- | Exercise 8
whatWentWrongEnhanced :: String -> [LogMessage] -> [String]
whatWentWrongEnhanced s xs = map logMessageString $ messagesAbout s (filter isImportantMsg xs)
