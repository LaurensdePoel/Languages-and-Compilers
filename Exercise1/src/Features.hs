module Features where

import Calendar
import Data.Data (maxConstrIndex)
import Data.List as P
import Data.Map as M
import DateTime
import Text.PrettyPrint.Boxes


--------------------------------------------------------------------------------------------------
-- Exercise 9
-- To test all the functions of exercise 9:

-- countEvents testCalendar -> 5 events in test calendar

-- findEvents testStartDate testCalendar

-- checkOverlapping testCalendar

-- timeSpent "niks" testCalendar 
-- timeSpent "Bastille" testCalendar 

countEvents :: Calendar -> Int
countEvents Calendar {events = _events} = length _events

findEvents :: DateTime -> Calendar -> [Event]
findEvents datetime Calendar {events = _events} =
  P.filter (isTimeBetweenEvent datetime) _events

checkOverlapping :: Calendar -> Bool
checkOverlapping Calendar {events = _events} =
  any
    ( \event ->
        let (start, end) = (getEventStartDateTime event, getEventEndDateTime event)
         in any (\_event -> isTimeBetweenEvent start _event && _event /= event) _events
              || any (\_event -> isTimeBetweenEvent end _event && _event /= event) _events
    )
    _events

timeSpent :: String -> Calendar -> Int
timeSpent checkSummary Calendar {events = _events} = P.foldr (\x res -> res + eventTime x) 0 filteredEvents `div` 60
  where
    filteredEvents = P.filter (\event -> getEventSummary event == checkSummary) _events
    eventTime :: Event -> Int
    eventTime event = dateTimeToSeconds end - dateTimeToSeconds start
      where
        (start, end) = (getEventStartDateTime event, getEventEndDateTime event)

----------------------------------------------------------------------------------------------------
-- To test exercise 10 run the following command
-- putStrLn $ ppMonth (Year 1997) (Month 7) testCalendar

-- In this example the times are orded from top to down on each day. 
-- When events last multible days it is carried over to the next day. 
ppMonth :: Year -> Month -> Calendar -> String
ppMonth year month Calendar {events = _events} =
    P.concatMap (\week -> printWeek week year month) weeks ++ printBorder
  where
    weeks = createWeeks year month

----------------------------------------------------------------------------------------------------
-- Helper functions exercise 9
----------------------------------------------------------------------------------------------------
testStartDate :: DateTime
testStartDate = DateTime (Date (Year 1997) (Month 07) (Day 14)) (Time (Hour 17) (Minute 00) (Second 00)) True

testInbetweenDate :: DateTime
testInbetweenDate = DateTime (Date (Year 1997) (Month 07) (Day 14)) (Time (Hour 20) (Minute 05) (Second 10)) True

testEndDate :: DateTime
testEndDate = DateTime (Date (Year 1997) (Month 07) (Day 15)) (Time (Hour 03) (Minute 00) (Second 00)) True

testOutSideDate :: DateTime
testOutSideDate = DateTime (Date (Year 1997) (Month 05) (Day 15)) (Time (Hour 04) (Minute 00) (Second 00)) True

isTimeBetweenEvent :: DateTime -> Event -> Bool
isTimeBetweenEvent checkDate event =
  checkDate >= start && checkDate < end
  where
    (start, end) = (getEventStartDateTime event, getEventEndDateTime event)

getEventStartDateTime :: Event -> DateTime
getEventStartDateTime Event {eventprops = (x : xs)} =
  case x of
    (DTSTART x) -> x
    _ -> getEventStartDateTime Event {eventprops = xs}

getEventEndDateTime :: Event -> DateTime
getEventEndDateTime Event {eventprops = (x : xs)} =
  case x of
    (DTEND x) -> x
    _ -> getEventEndDateTime Event {eventprops = xs}

getEventSummary :: Event -> String
getEventSummary Event {eventprops = (x : xs)} =
  case x of
    (SUMMARY x) -> x
    _ -> getEventSummary Event {eventprops = xs}

----------------------------------------------------------------------------------------------------
-- Helper functions exercise 10
----------------------------------------------------------------------------------------------------

printBorder :: String
printBorder = concat (replicate 7 ("+" ++ replicate 13 '-')) ++ "+" ++ "\n"

printField :: Int -> String -> String
printField n xs = xs ++ whiteSpace
  where
    numberOfWhitespace = n - length xs
    whiteSpace = replicate numberOfWhitespace ' '

printRow :: [String] -> String
printRow xs = "|" ++ intercalate "|" (P.map (printField 13) xs) ++ "|" ++ "\n"

printHeader :: [Int] -> String
printHeader xs = "|" ++ intercalate "|" (P.map (printDayNr 13) xs) ++ "|" ++ "\n"

printDayNr :: Int -> Int -> String
printDayNr n dayNr
  | dayNr > 0 = show dayNr ++ whiteSpace
  | otherwise = "             "
  where
    numberOfWhitespace
      | dayNr < 10 = n - 1
      | otherwise = n - 2
    whiteSpace = replicate numberOfWhitespace ' '

printWeek :: [Int] -> Year -> Month -> String
printWeek xs year month = printBorder ++ printHeader xs ++ printWeekEvents xs year month

printWeekEvents :: [Int] -> Year -> Month -> String
printWeekEvents xs year month = concatMap printRow (transpose (convertWeekToMaxSize (getEventsInWeek xs)))
  where
    allEvents = createCalendarDays2 year month testCalendar

    getEventsInWeek :: [Int] -> [[String]]
    getEventsInWeek = P.map (`getDayEvents` allEvents)

    convertWeekToMaxSize :: [[String]] -> [[String]]
    convertWeekToMaxSize xss = P.map addUntilMax xss
      where
        maxDepth :: Int
        maxDepth = maximum $ P.map length xss

        addUntilMax :: [String] -> [String]
        addUntilMax list
          | length list < maxDepth = addUntilMax $ list ++ [""]
          | otherwise = list

-- create all days and events
createWeeks :: Year -> Month -> [[Int]]
createWeeks year month = getWeek allDays
    where
        allDays = createDaysheading year month
        getWeek :: [Int] -> [[Int]]
        getWeek xs  | length xs >= 7 = P.take 7 xs : getWeek (P.drop 7 xs)
                    | otherwise = [ xs ++ P.take (7 - length xs) [0,0 ..]]

createDaysheading :: Year -> Month -> [Int]
createDaysheading year month = getAllDays
  where
    getAllDays = P.take amountOfDays [1, 2 ..]
    amountOfDays = nrDaysOfMonth year month


createCalendarDays2 :: Year -> Month -> Calendar -> Map Int [String]
createCalendarDays2 year' month' Calendar {events = _events} = P.foldr addEvent M.empty _events
  where
    addEvent :: Event -> Map Int [String] -> Map Int [String]
    addEvent event eventsPerDayList
      | year startDate /= year' || month startDate /= month' = eventsPerDayList -- check if event isn't itself, if so don't add it
      | startDate > endDate = eventsPerDayList -- if start date is after end date, don't add it
      | month startDate /= month endDate =  addUntilEndDay startDate (toLastDayOfTheMonth startDate) (fst eventTimes,midnight) eventsPerDayList -- if event goes into next month only put the event in map untill the end of the month
      | startDay /= endDay = addUntilEndDay startDate endDate eventTimes eventsPerDayList

      | otherwise = addDayEvent startDay eventTimes eventsPerDayList
      where
        (startDate, endDate) = (date $ getEventStartDateTime event, date $ getEventEndDateTime event)
        (startDay, endDay) = (day startDate, day endDate)
        eventTimes = (time $ getEventStartDateTime event, time $ getEventEndDateTime event)
    
    addUntilEndDay :: Date -> Date -> (Time,Time) -> Map Int [String] -> Map Int [String]
    addUntilEndDay currentDate endDate times eventsPerDayList 
        | currentDate < endDate = addUntilEndDay (addDay currentDate) endDate (morning, snd times) $ addDayEvent (day currentDate) (fst times, midnight) eventsPerDayList
        | otherwise = addDayEvent (day currentDate) times eventsPerDayList

    addDayEvent :: Day -> (Time,Time) -> Map Int [String] -> Map Int [String]
    addDayEvent currentDay (startTime, endTime) eventsPerDayList = case M.lookup (runDay currentDay) eventsPerDayList of
            Nothing -> M.insert (runDay currentDay) [timeToString startTime ++ " - " ++ timeToString endTime] eventsPerDayList
            Just x -> M.insert (runDay currentDay) ((timeToString startTime ++ " - " ++ timeToString endTime) : x) eventsPerDayList
            where
                timeToString :: Time -> String
                timeToString Time {hour = _hour, minute = _minute} = showNum (runHour _hour) ++ ":" ++ showNum (runMinute _minute)
                  where
                    showNum :: Int -> String
                    showNum x
                      | x < 9 = "0" ++ show x
                      | otherwise = show x

getDayEvents :: Int -> Map Int [String] -> [String]
getDayEvents key map =
  case M.lookup key map of
    Nothing -> []
    Just x -> P.sort x
