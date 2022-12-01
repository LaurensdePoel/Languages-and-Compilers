module Calendar where

import Data.Time.Calendar (dayOfWeek, gregorianMonthLength, isLeapYear)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
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
  { calprop :: [CalProp],
    events :: [Event]
  }
  deriving (Eq, Ord, Show)

data CalProp = PRODID String | VERSION String deriving (Eq, Ord, Show)

-- newtype ProdID = PRODID String

-- newtype Version = VERSION String

data Event = Event
  { eventprops :: [EventProp]
  }
  deriving (Eq, Ord, Show)

data EventProp = DTSTAMP DateTime | UID String | DTSTART DateTime | DTEND DateTime | DESCRIPTION String | SUMMARY String | LOCATION String deriving (Eq, Ord, Show)

-- newtype DtStamp = DTSTAMP DateTime

-- newtype Uid = UID String

-- newtype DtStart = DTSTART DateTime

-- newtype DtEnd = DTEND DateTime

-- newtype Description = DESCRIPTION String

-- newtype Summary = SUMMARY String

-- newtype Location = LOCATION String

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
  | TBEGIN
  | TEND
  | TVCALENDAR
  | TVEVENT
  deriving (Eq, Ord, Show)

dateTimeToSeconds :: DateTime -> Int
dateTimeToSeconds DateTime {date = Date {year = _year, month = _month, day = _day}, time = Time {hour = _hour, minute = _minute, second = _second}} =
  runSecond _second
    + runMinute _minute * 60
    + runHour _hour * 60 * 60
    + runDay _day * 24 * 60 * 60
    + nrDaysOfMonth _year _month * 24 * 60 * 60
    + nrDaysOfYear (runYear _year) * 24 * 60 * 60

nrDaysOfMonth :: Year -> Month -> Int
nrDaysOfMonth year month = gregorianMonthLength (fromIntegral (runYear year)) (runMonth month)

nrDaysOfYear :: Int -> Int
nrDaysOfYear year
  | isLeapYear (fromIntegral year - 1970) = 366
  | otherwise = 365

tDateTime, tText, tKey :: Parser Char Token
tDateTime = TDateTime <$> parseDateTime <* symbol '\n'
tText = TText <$> greedy1 (satisfy stillChars) <* symbol '\n'
  where
    stillChars :: Char -> Bool
    stillChars c = c /= '\n' && c /= '\r'
tKey =
  TKey TDTStamp <$ token "DTSTAMP:"
    <|> TKey TUID <$ token "UID:"
    <|> TKey TDTStart <$ token "DTSTART:"
    <|> TKey TDTEnd <$ token "DTEND:"
    <|> TKey TDescription <$ token "DESCRIPTION:"
    <|> TKey TSummary <$ token "SUMMARY:"
    <|> TKey TLocation <$ token "LOCATION:"
    <|> TKey TProdID <$ token "PRODID:"
    <|> TKey TVersion <$ token "VERSION:"
    <|> TKey TBEGIN <$ token "BEGIN:"
    <|> TKey TEND <$ token "END:"
    <|> TKey TVCALENDAR <$ token "VCALENDAR"
    <|> TKey TVEVENT <$ token "VEVENT"

anyToken :: Parser Char Token
anyToken = tDateTime <|> tKey <|> tText

scanCalendar :: Parser Char [Token]
scanCalendar = greedy anyToken -- <* eof

tes = run scanCalendar "BEGIN:VCALENDAR\nPRODID:2\nVERSION:1\nBEGIN:VEVENT\nDTSTAMP:19970610T172345Z\nUID:test\nDTSTART:19970610T172345Z\nDTEND:19970710T172345Z\nDESCRIPTION:test\nSUMMARY:test\nLOCATION:test\nEND:VEVENT\nEND:VCALENDAR\n" -- "DTSTART:19970610T172345ZDTEND:19970610T172345Z"

tes3 = run scanCalendar "PRODID:test123\nVERSION:2.0\n"

tes4 = run scanCalendar "DTSTART:19970610T172345Z\nDTEND:19970710T172345Z\n"

tes2 :: Maybe Calendar
tes2 = recognizeCalendar "PRODID:prodid2\nVERSION:vers1\nDTSTAMP:19970610T172345Z\nUID:uid\nDTSTART:19970610T172345Z\nDTEND:19970710T172345Z\nDESCRIPTION:descr\nSUMMARY:summ\nLOCATION:loc\n"

dateTime :: Parser Token DateTime
dateTime = fromDateTime <$> satisfy isDateTime

isDateTime :: Token -> Bool
isDateTime (TDateTime _) = True
isDateTime _ = False

fromDateTime :: Token -> DateTime
fromDateTime (TDateTime x) = x
fromDateTime _ = error "fromDateTime"

text :: Parser Token String
text = fromText <$> satisfy isText

isText :: Token -> Bool
isText (TText _) = True
isText _ = False

fromText :: Token -> String
fromText (TText x) = x
fromText _ = error "fromText"

keyWord :: Parser Token TokenKey
keyWord = fromKey <$> satisfy isKey

isKey :: Token -> Bool
isKey (TKey _) = True
isKey _ = False

fromKey :: Token -> TokenKey
fromKey (TKey x) = x
fromKey _ = error "fromKey"

-- event :: Parser Token Event
-- event = Event <$ keyWord <*> dateTime <* keyWord <*> text <* keyWord <*> dateTime <* keyWord <*> dateTime <* keyWord <*> text <* keyWord <*> text <* keyWord <*> text

parseCalendar :: Parser Token Calendar
parseCalendar = undefined -- Calendar <$ keyWord <*> text <* keyWord <*> text <*> many event

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
testStartDate :: DateTime
testStartDate = DateTime (Date (Year 1997) (Month 07) (Day 14)) (Time (Hour 17) (Minute 00) (Second 00)) True

testInbetweenDate :: DateTime
testInbetweenDate = DateTime (Date (Year 1997) (Month 07) (Day 14)) (Time (Hour 20) (Minute 05) (Second 10)) True

testEndDate :: DateTime
testEndDate = DateTime (Date (Year 1997) (Month 07) (Day 15)) (Time (Hour 03) (Minute 00) (Second 00)) True

testOutSideDate :: DateTime
testOutSideDate = DateTime (Date (Year 1997) (Month 05) (Day 15)) (Time (Hour 04) (Minute 00) (Second 00)) True

testCalendar :: Calendar
testCalendar =
  Calendar
    { calprop = [PRODID "-//hacksw/handcal//NONSGML v1.0//EN", VERSION "2.0"],
      events =
        [ Event
            { eventprops =
                [ DTSTAMP (DateTime (Date (Year 1997) (Month 06) (Day 10)) (Time (Hour 17) (Minute 23) (Second 45)) True),
                  UID "19970610T172345Z-AF23B2@example.com",
                  DTSTART (DateTime (Date (Year 1997) (Month 07) (Day 14)) (Time (Hour 17) (Minute 00) (Second 00)) True),
                  DTEND (DateTime (Date (Year 1997) (Month 07) (Day 14)) (Time (Hour 19) (Minute 00) (Second 00)) True),
                  SUMMARY "Bastille"
                ]
            }
        ]
    }

testEvent :: Event
testEvent =
  Event
    { eventprops =
        [ DTSTAMP (DateTime (Date (Year 1997) (Month 06) (Day 10)) (Time (Hour 17) (Minute 23) (Second 45)) True),
          UID "19970610T172345Z-AF23B2@example.com",
          DTEND (DateTime (Date (Year 1997) (Month 07) (Day 14)) (Time (Hour 19) (Minute 20) (Second 00)) True),
          DTSTART (DateTime (Date (Year 1997) (Month 07) (Day 14)) (Time (Hour 17) (Minute 00) (Second 00)) True),
          SUMMARY "Bastille"
        ]
    }

-- { prodid = "-//hacksw/handcal//NONSGML v1.0//EN",
--   version = "2.0",
--   events =
--     [ Event
--         { dtstamp = DateTime (Date (Year 1997) (Month 06) (Day 10)) (Time (Hour 17) (Minute 23) (Second 45)) True,
--           uid = "19970610T172345Z-AF23B2@example.com",
--           dtstart = DateTime (Date (Year 1997) (Month 07) (Day 14)) (Time (Hour 17) (Minute 00) (Second 00)) True,
--           dtend = DateTime (Date (Year 1997) (Month 07) (Day 15)) (Time (Hour 03) (Minute 00) (Second 00)) True,
--           description = "",
--           summary = "Bastille",
--           location = ""
--         },
--       Event
--         { dtstamp = DateTime (Date (Year 1997) (Month 02) (Day 10)) (Time (Hour 07) (Minute 23) (Second 45)) True,
--           uid = "1997061175Z-AF23B2@example.com",
--           dtstart = DateTime (Date (Year 1997) (Month 07) (Day 14)) (Time (Hour 17) (Minute 00) (Second 00)) True,
--           dtend = DateTime (Date (Year 1997) (Month 07) (Day 14)) (Time (Hour 05) (Minute 00) (Second 00)) True,
--           description = "",
--           summary = "Bastille  Party",
--           location = ""
--         }
--     ]
-- }

printCalendar :: Calendar -> String
printCalendar Calendar {calprop = _calprop, events = _events} =
  "BEGIN:VCALENDAR \r\n"
    ++ concatMap calPropToString _calprop
    ++ concatMap eventToString _events
    ++ "END:VCALENDAR \r\n"
  where
    calPropToString :: CalProp -> String
    calPropToString (PRODID x) = "PRODID:" ++ x ++ "\r\n"
    calPropToString (VERSION x) = "VERSION:" ++ x ++ "\r\n"

    eventToString :: Event -> String
    eventToString Event {eventprops = _eventprops} =
      "BEGIN:VEVENT \r\n"
        ++ concatMap eventPropToString _eventprops
        ++ "END:VEVENT \r\n"
      where
        eventPropToString :: EventProp -> String
        eventPropToString (DTSTAMP x) = "DTSTAMP:" ++ printDateTime x ++ "\r\n"
        eventPropToString (UID x) = "UID:" ++ x ++ "\r\n"
        eventPropToString (DTSTART x) = "DTSTART:" ++ printDateTime x ++ "\r\n"
        eventPropToString (DTEND x) = "DTEND:" ++ printDateTime x ++ "\r\n"
        eventPropToString (SUMMARY x) = "SUMMARY:" ++ x ++ "\r\n"
        eventPropToString (DESCRIPTION x) = "DESCRIPTION:" ++ x ++ "\r\n"
        eventPropToString (LOCATION x) = "LOCATION:" ++ x ++ "\r\n"

-- printCalendar Calendar {prodid = _prodid, version = _version, events = _events} =
--   "BEGIN:VCALENDAR \r\n"
--     ++ "PRODID:"
--     ++ _prodid
--     ++ "\r\n"
--     ++ "VERSION:"
--     ++ _version
--     ++ "\r\n"
--     ++ concatMap printEvent _events
--     ++ "END:VCALENDAR \r\n"
--   where
--     printEvent :: Event -> String
--     printEvent
--       Event
--         { dtstamp = _dtstamp,
--           uid = _uid,
--           dtstart = _dtstart,
--           dtend = _dtend,
--           description = _description,
--           summary = _summary,
--           location = _location
--         } =
--         "BEGIN:VEVENT \r\n"
--           ++ "SUMMARY:"
--           ++ _summary
--           ++ "\r\n"
--           ++ "UID:"
--           ++ _uid
--           ++ "\r\n"
--           ++ "LOCATION:"
--           ++ _location
--           ++ "\r\n"
--           ++ "DESCRIPTION:"
--           ++ _description
--           ++ "\r\n"
--           ++ "DTSTAMP:"
--           ++ printDateTime _dtstamp
--           ++ "\r\n"
--           ++ "DTSTART:"
--           ++ printDateTime _dtstart
--           ++ "\r\n"
--           ++ "DTEND:"
--           ++ printDateTime _dtend
--           ++ "\r\n"
--           ++ "END:VEVENT \r\n"

tes5 = case tes2 of
  Nothing -> ""
  Just x -> printCalendar x

tes6 = printCalendar testCalendar
