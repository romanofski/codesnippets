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
-- ValidLM (LogMessage Info 29 "I 29 la")
-- >>> toLogMessage ["Foo", "bar", "baz"]
-- InvalidLM String
toLogMessage :: [String] -> MaybeLogMessage
toLogMessage p@("I":_) = ValidLM (LogMessage (toError p) (toTimeStamp p) (unwords p))
-- parseMessage :: String -> MaybeLogMessage
-- parseMessage s = Control.Exception.catch (ValidLM (LogMessage (toError s) (toTimeStamp s) s)) (\err -> InvalidLM err)
