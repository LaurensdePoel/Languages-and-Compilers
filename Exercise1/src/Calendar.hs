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
  | TBEGINVCALENDAR
  | TENDVCALENDAR
  | TBEGINVEVENT
  | TENDVEVENT
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
    <|> TKey TBEGINVCALENDAR <$ token "BEGIN:VCALENDAR" <* symbol '\n'
    <|> TKey TENDVCALENDAR <$ token "END:VCALENDAR" <* symbol '\n'
    <|> TKey TBEGINVEVENT <$ token "BEGIN:VEVENT" <* symbol '\n'
    <|> TKey TENDVEVENT <$ token "END:VEVENT" <* symbol '\n'

anyToken :: Parser Char Token
anyToken = tDateTime <|> tKey <|> tText

scanCalendar :: Parser Char [Token]
scanCalendar = greedy anyToken -- <* eof

tes = run scanCalendar "BEGIN:VCALENDAR\nPRODID:2\nVERSION:1\nBEGIN:VEVENT\nDTSTAMP:19970610T172345Z\nUID:test\nDTSTART:19970610T172345Z\nDTEND:19970710T172345Z\nDESCRIPTION:test\nSUMMARY:test\nLOCATION:test\nEND:VEVENT\nEND:VCALENDAR\n" -- "DTSTART:19970610T172345ZDTEND:19970610T172345Z"

tes3 = run scanCalendar "PRODID:test123\nVERSION:2.0\n"

tes4 = run scanCalendar "DTSTART:19970610T172345Z\nDTEND:19970710T172345Z\n"

tes2 :: Maybe Calendar
tes2 = recognizeCalendar "BEGIN:VCALENDAR\nPRODID:2\nVERSION:1\nBEGIN:VEVENT\nDTSTAMP:19970610T172345Z\nUID:test\nDTSTART:19970610T172345Z\nDTEND:19970710T172345Z\nDESCRIPTION:test\nSUMMARY:test\nLOCATION:test\nEND:VEVENT\nEND:VCALENDAR\n" -- "PRODID:prodid2\nVERSION:vers1\nDTSTAMP:19970610T172345Z\nUID:uid\nDTSTART:19970610T172345Z\nDTEND:19970710T172345Z\nDESCRIPTION:descr\nSUMMARY:summ\nLOCATION:loc\n"

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

-- parseAnyEventProp :: Parser Token EventProp
-- parseAnyEventProp = parseDtStamp <|> parseDtStart <|>

parseEventProp :: Parser Token EventProp
parseEventProp = createEventProp <$> satisfy isKey <*> satisfy isValue -- (text <|> dateTime)

isValue :: Token -> Bool
isValue (TText _) = True
isValue (TDateTime _) = True
isValue _ = False

createEventProp :: Token -> Token -> EventProp
createEventProp (TKey TDTStamp) (TDateTime datetime) = DTSTAMP datetime
createEventProp (TKey TDTStart) (TDateTime datetime) = DTSTART datetime
createEventProp (TKey TDTEnd) (TDateTime datetime) = DTEND datetime
createEventProp (TKey TUID) (TText text) = UID text
createEventProp (TKey TSummary) (TText text) = SUMMARY text
createEventProp (TKey TDescription) (TText text) = DESCRIPTION text
createEventProp (TKey TLocation) (TText text) = LOCATION text
createEventProp _ _ = error "invalid token(s)."

parseCalProp :: Parser Token CalProp
parseCalProp = createCalProp <$> satisfy isKey <*> satisfy isValue

createCalProp :: Token -> Token -> CalProp
createCalProp (TKey TProdID) (TText text) = PRODID text
createCalProp (TKey TVersion) (TText text) = VERSION text
createCalProp _ _ = error "invalid tokens"

getEvent :: Parser Token Event
-- getEvent = Event <$ keyWord <*> greedy parseEventProp <* keyWord -- <* satisfy isNotEventEndToken <* keyWord
getEvent = event -- if isValidEvent event then event else event
  where
    event = (\p -> if isValidEvent (Event p) then Event p else error "invalid event") <$ keyWord <*> greedy parseEventProp <* keyWord

-- isNotEventEndToken :: Token -> Bool
-- isNotEventEndToken (TKey TENDVEVENT) = False
-- isNotEventEndToken _ = True

-- getEvent = (\p -> Event <$> many parseEventProp ) <$> (satisfy isEventEndToken)
-- [TKey TDTStamp..,]
-- 1. map over alle tokens heen die we krijgen
-- 2. voor elke 2 moeten we een event property maken
-- 3. dit moet gedaan worden tot de hele lijst leeg is die we krijgen.
-- 4. hierdoor een event met een lijst van eventprops

-- get event maakt een event
-- event is een lijst van eventprops
-- satisfy isEventEndToken pakt alle tokens tot aan endevent token
-- parseEventToken pakt elke keer 2 tokens (keyword text/dateTime)

-- event :: Parser Token Event
-- event = Event <$ keyWord <*> dateTime <* keyWord <*> text <* keyWord <*> dateTime <* keyWord <*> dateTime <* keyWord <*> text <* keyWord <*> text <* keyWord <*> text

-- beginCalendar :: Parser Token TokenKey
-- beginCalendar = greedy (satisfy isBeginToken)
--   where
--     isBeginToken :: Token -> Bool
--     isBeginToken (TKey TBEGINVCALENDAR) = False
--     isBeginToken _ = True

-- parseCalProp ::
-- parseCalProp =

-- parseCalendarData = parseCalProp <|> getEvent

parseCalProps :: Parser Token [CalProp]
parseCalProps = (\p -> if isValidCalProps p then p else error "invalid cal properties") <$> greedy parseCalProp -- <* satisfy isNotBeginEvent

-- isNotBeginEvent :: Token -> Bool
-- isNotBeginEvent (TKey TBEGINVEVENT) = False
-- isNotBeginEvent _ = True

parseEvents :: Parser Token [Event]
parseEvents = greedy getEvent

-- parseEvents :: Parser Token [Event]
-- parseEvents = greedy (ev)
--   where
--     ev = case getEvent of
--           Nothing -> getEvent <* eof
--           Just x -> x

parseCalendar :: Parser Token Calendar
parseCalendar = Calendar <$ keyWord <*> parseCalProps <*> parseEvents <* keyWord -- Calendar <$ beginCalendar <*>   -- <$ keyWord <*> text <* keyWord <*> text <*> many event

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- tes7 = (UID "test") `elem` [UID "test", SUMMARY "testtest"]

-- tes8 = (length $ filter (\p -> p == (UID "test")) [UID "test", SUMMARY "testtest"]) == 1

-- occurrences :: EventProp -> [EventProp] -> Int
-- occurrences ep [] = 0
-- occurrences ep (x : xs) =
--   case x of
--     (UID _) -> 1 + occurrences ep xs
--     _ -> occurrences ep xs

tes9 = isValidEvent Event {eventprops = [UID "test", SUMMARY "testtest", DTSTAMP testStartDate, DTSTART testStartDate, DTEND testStartDate]}

data EventPropsOccurrences = EventPropsOccurrences
  { rDtStamp :: Int,
    rDtStart :: Int,
    rDtEnd :: Int,
    rUID :: Int,
    rSummary :: Int,
    nrdescription :: Int,
    nrlocation :: Int
  }

data CalPropsOccurrences = CalPropsOccurrences
  { rProdID :: Int,
    rVersion :: Int
  }

-- Update amount off times the required props occur
eventPropOccurrences :: EventPropsOccurrences -> [EventProp] -> EventPropsOccurrences
eventPropOccurrences occurrences [] = occurrences
eventPropOccurrences occurrences (x : xs) =
  case x of
    (DTSTAMP _) -> eventPropOccurrences (occurrences {rDtStamp = 1 + rDtStamp occurrences}) xs
    (DTSTART _) -> eventPropOccurrences (occurrences {rDtStart = 1 + rDtStart occurrences}) xs
    (DTEND _) -> eventPropOccurrences (occurrences {rDtEnd = 1 + rDtEnd occurrences}) xs
    (UID _) -> eventPropOccurrences (occurrences {rUID = 1 + rUID occurrences}) xs
    (SUMMARY _) -> eventPropOccurrences (occurrences {rSummary = 1 + rSummary occurrences}) xs
    (DESCRIPTION _) -> eventPropOccurrences (occurrences {nrdescription = 1 + nrdescription occurrences}) xs
    (LOCATION _) -> eventPropOccurrences (occurrences {nrlocation = 1 + nrlocation occurrences}) xs

-- Update amount off times the calprops occur
calPropOccurrences :: CalPropsOccurrences -> [CalProp] -> CalPropsOccurrences
calPropOccurrences occurrences [] = occurrences
calPropOccurrences occurrences (x : xs) =
  case x of
    (VERSION _) -> calPropOccurrences (occurrences {rVersion = 1 + rVersion occurrences}) xs
    (PRODID _) -> calPropOccurrences (occurrences {rProdID = 1 + rProdID occurrences}) xs

-- Check if all required event props occur only once
isValidEvent :: Event -> Bool
isValidEvent Event {eventprops = _eventprops} =
  1 == rDtStamp occurrences
    && 1 == rDtStart occurrences
    && 1 == rDtEnd occurrences
    && 1 == rUID occurrences
    && 1 == rSummary occurrences
    && (1 == nrdescription occurrences || 0 == nrdescription occurrences)
    && (1 == nrlocation occurrences || 0 == nrlocation occurrences)
  where
    occurrences :: EventPropsOccurrences
    occurrences =
      eventPropOccurrences
        EventPropsOccurrences
          { rDtStamp = 0,
            rDtStart = 0,
            rDtEnd = 0,
            rUID = 0,
            rSummary = 0,
            nrdescription = 0,
            nrlocation = 0
          }
        _eventprops

-- Check if all required event props occur only once
isValidCalProps :: [CalProp] -> Bool
isValidCalProps prop =
  1 == rVersion occurrences
    && 1 == rProdID occurrences
  where
    occurrences :: CalPropsOccurrences
    occurrences =
      calPropOccurrences
        CalPropsOccurrences
          { rVersion = 0,
            rProdID = 0
          }
        prop

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
