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
{
  prodid :: String, -- calander id?
  version :: String,
  event :: [Event]
}
  deriving (Eq, Ord, Show)

data Event = Event
{
  dtstamp     :: DateTime,
  uid         :: String,
  dtstart     :: DateTime,
  dtend       :: DateTime,
  description :: String,
  summary     :: String,
  location    :: String
}
  deriving (Eq, Ord, Show)

-- Exercise 7
data Token = Token
  deriving (Eq, Ord, Show)

scanCalendar :: Parser Char [Token]
scanCalendar = undefined

parseCalendar :: Parser Token Calendar
parseCalendar = undefined

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined
