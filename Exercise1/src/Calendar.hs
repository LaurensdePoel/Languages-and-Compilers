module Calendar where

import DateTime
import ParseLib.Abstract
import Prelude hiding (sequence, ($>), (*>), (<$), (<*))
import Data.Time.Calendar (isLeapYear, gregorianMonthLength, dayOfWeek)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)

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
    events :: [Event]
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


dateTimeToSeconds :: DateTime -> Int
dateTimeToSeconds DateTime {date=Date {year=_year, month=_month, day=_day}, time= Time {hour = _hour, minute = _minute, second = _second}} = 
    runSecond _second +
    runMinute _minute * 60 + 
    runHour _hour * 60 *60 + 
    runDay _day * 24 * 60 * 60 + 
    nrDaysOfMonth _year  _month * 24 * 60 * 60 +
    nrDaysOfYear (runYear _year) * 24 * 60 * 60 


nrDaysOfMonth :: Year -> Month -> Int
nrDaysOfMonth year month = gregorianMonthLength (fromIntegral (runYear year)) (runMonth month)
nrDaysOfYear :: Int -> Int
nrDaysOfYear year | isLeapYear (fromIntegral year - 1970) = 366
              | otherwise = 365

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



parseCalendar :: Parser Token Calendar
parseCalendar = undefined

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
testCalendar = Calendar {
  prodid= "-//hacksw/handcal//NONSGML v1.0//EN", 
  version= "2.0", 
  events= [Event {
    dtstamp= DateTime (Date (Year 1997) (Month 06) (Day 10)) (Time (Hour 17) (Minute 23) (Second 45)) True,
    uid=  "19970610T172345Z-AF23B2@example.com", 
    dtstart= DateTime (Date (Year 1997) (Month 07) (Day 14)) (Time (Hour 17) (Minute 00) (Second 00)) True, 
    dtend= DateTime (Date (Year 1997) (Month 07) (Day 15)) (Time (Hour 03) (Minute 00) (Second 00)) True, 
    description="", 
    summary= "Bastille", 
    location=""},
    Event {
    dtstamp= DateTime (Date (Year 1997) (Month 02) (Day 10)) (Time (Hour 07) (Minute 23) (Second 45)) True,
    uid=  "1997061175Z-AF23B2@example.com", 
    dtstart= DateTime (Date (Year 1997) (Month 07) (Day 14)) (Time (Hour 17) (Minute 00) (Second 00)) True, 
    dtend= DateTime (Date (Year 1997) (Month 07) (Day 14)) (Time (Hour 05) (Minute 00) (Second 00)) True, 
    description="", 
    summary= "Bastille  Party", 
    location=""}]
    }

printCalendar :: Calendar -> String
printCalendar Calendar {prodid=_prodid, version=_version, events =_events} = 
  "BEGIN:VCALENDAR \r\n" ++
  "PRODID:" ++ _prodid ++ "\r\n" ++
  "VERSION:" ++ _version ++ "\r\n" ++
  concatMap printEvent _events ++
  "END:VCALENDAR \r\n"
  where
    printEvent :: Event -> String
    printEvent Event {
      dtstamp=_dtstamp, 
      uid=_uid, 
      dtstart=_dtstart, 
      dtend=_dtend, 
      description=_description, 
      summary=_summary, 
      location=_location} =
        "BEGIN:VEVENT \r\n" ++
        "SUMMARY:" ++ _summary ++ "\r\n" ++
        "UID:" ++ _uid ++ "\r\n" ++
        "LOCATION:" ++ _location ++ "\r\n" ++
        "DESCRIPTION:" ++ _description ++ "\r\n" ++
        "DTSTAMP:" ++ printDateTime _dtstamp ++ "\r\n" ++
        "DTSTART:" ++ printDateTime _dtstart ++ "\r\n" ++
        "DTEND:" ++ printDateTime _dtend ++ "\r\n" ++
        "END:VEVENT \r\n"



