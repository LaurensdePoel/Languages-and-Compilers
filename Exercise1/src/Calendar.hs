module Calendar where

import DateTime
import ParseLib.Abstract
import Prelude hiding (sequence, ($>), (*>), (<$), (<*))

-- Exercise 6

{-

 BEGIN:VCALENDAR
 PRODID:-//hacksw/handcal//NONSGML v1.0//EN
 VERSION:2.0
 BEGIN:VEVENT
 SUMMARY:Bastille Day Party
 UID:19970610T172345Z-AF23B2@example.com
 DTSTAMP:19970610T172345Z
 DTSTART:19970714T170000Z
 DTEND:19970715T040000Z
 END:VEVENT
 END:VCALENDAR

-}

data Calendar = Calendar
  { prodid :: String, -- calander id?
    version :: String,
    event :: [Event]
  }
  deriving (Eq, Ord, Show)

data Event = Event
  { dtstamp :: DateTime,
    uid :: String,
    dtstart :: DateTime,
    dtend :: DateTime,
    description :: String,
    summary :: String,
    location :: String
  }
  deriving (Eq, Ord, Show)

-- Exercise 7
data Token
  = TDateTime DateTime
  | TText String
  | TKey TokenKey
  deriving (Eq, Ord, Show)

data TokenKey
  = TDTStamp
  | TUID
  | TDTStart
  | TDTEnd
  | TDescription
  | TSummary
  | TLocation
  | TProdID
  | TVersion
  deriving (Eq, Ord, Show)

tDateTime, tText, tKey :: Parser Char Token
tDateTime = TDateTime <$> parseDateTime
tText = TText <$> greedy1 (satisfy stillChars)
  where
    stillChars :: Char -> Bool
    stillChars c = c /= '\n' || c /= '\r'
tKey =
  const (TKey TDTStamp) <$> token "DTSTAMP:"
    <|> const (TKey TUID) <$> token "UID:"
    <|> const (TKey TDTStart) <$> token "DTSTART:"
    <|> const (TKey TDTEnd) <$> token "DTEND:"
    <|> const (TKey TDescription) <$> token "DESCRIPTION:"
    <|> const (TKey TSummary) <$> token "SUMMARY:"
    <|> const (TKey TLocation) <$> token "LOCATION:"
    <|> const (TKey TProdID) <$> token "PRODID:"
    <|> const (TKey TVersion) <$> token "VERSION:"

anyToken :: Parser Char Token
anyToken = tDateTime <|> tText <|> tKey

scanCalendar :: Parser Char [Token]
scanCalendar = greedy anyToken <* eof

dateTime :: Parser Token DateTime
dateTime = fromDateTime <$> satisfy isDateTime

parseCalendar :: Parser Token Calendar
parseCalendar = parse

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined
