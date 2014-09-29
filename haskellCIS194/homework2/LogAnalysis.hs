{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- | Exercise 1
-- >>> toError ["I", "29", "la", "la"]
-- Info
-- >>> toError ["W", "29", "la"]
-- Warning
-- >>> toError ["E", "2", "562", "help", "help"]
-- Error 2
toError :: [String] -> MessageType
toError ("I":_) = Info
toError ("W":_) = Warning
toError ("E":x:_) = Error (read x :: Int)
toError _ = error "Unkown Error code"


-- |
-- >>> toTimeStamp ["I", "29", "la"]
-- 29
-- >>> toTimeStamp ["E","2","30","foo"]
-- 30
toTimeStamp :: [String] -> TimeStamp
toTimeStamp xs = case xs of
                ("E":_:x:_) -> read x :: TimeStamp
                (_:x:_) -> read x :: TimeStamp

-- |
-- >>> toLogMessage ["I", "29", "la"]
-- ValidLM (LogMessage Info 29 "la")
-- >>> toLogMessage ["E", "2", "29", "help"]
-- ValidLM (LogMessage (Error 2) 29 "help")
-- >>> toLogMessage ["Foo", "bar", "baz"]
-- InvalidLM "Foo bar baz"
toLogMessage :: [String] -> MaybeLogMessage
toLogMessage p@("I":_) = ValidLM (LogMessage (toError p) (toTimeStamp p) (unwords $ drop 2 p))
toLogMessage p@("E":_) = ValidLM (LogMessage (toError p) (toTimeStamp p) (unwords $ drop 3 p))
toLogMessage p = InvalidLM (unwords p)

-- |
-- >>> parseMessage "E 2 562 help help"
-- ValidLM (LogMessage (Error 2) 562 "help help")
-- >>> parseMessage "I 29 la la la"
-- ValidLM (LogMessage Info 29 "la la la")
-- >>> parseMessage "Not the right format"
-- InvalidLM "Not the right format"
parseMessage :: String -> MaybeLogMessage
parseMessage s = toLogMessage (words s)
