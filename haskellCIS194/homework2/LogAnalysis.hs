{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

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
