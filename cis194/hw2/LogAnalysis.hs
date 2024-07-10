module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage "" = Unknown ""
parseMessage ('I':' ':([ts]:' ':msg)) = LogMessage Info convertStringToNumber ts) msg

convertStringToNumber :: String -> Int
convertStringToNumber s = 10

getMessageType :: Char -> MessageType
getMessageType 'I' = Info
getMessageType 'W' = Warning
getMessageType 'E' = Error 0
