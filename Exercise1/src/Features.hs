module Features where

import Calendar
import Data.Data (maxConstrIndex)
import Data.List as P
import Data.Map as M
import DateTime
import Text.PrettyPrint.Boxes

-- Exercise 9
countEvents :: Calendar -> Int
countEvents Calendar {events = _events} = length _events

findEvents :: DateTime -> Calendar -> [Event]
findEvents datetime Calendar {events = _events} =
  P.filter (isTimeBetweenEvent datetime) _events

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

checkOverlapping :: Calendar -> Bool
checkOverlapping Calendar {events = _events} =
  any
    ( \event ->
        let (start, end) = (getEventStartDateTime event, getEventEndDateTime event)
         in any (\_event -> isTimeBetweenEvent start _event && _event /= event) _events
              || any (\_event -> isTimeBetweenEvent end _event && _event /= event) _events
    )
    _events

getEventSummary :: Event -> String
getEventSummary Event {eventprops = (x : xs)} =
  case x of
    (SUMMARY x) -> x
    _ -> getEventSummary Event {eventprops = xs}

timeSpent :: String -> Calendar -> Int
timeSpent checkSummary Calendar {events = _events} = P.foldr (\x res -> res + eventTime x) 0 filteredEvents `div` 60
  where
    filteredEvents = P.filter (\event -> getEventSummary event == checkSummary) _events
    eventTime :: Event -> Int
    eventTime event = dateTimeToSeconds end - dateTimeToSeconds start
      where
        (start, end) = (getEventStartDateTime event, getEventEndDateTime event)

-- Exercise 10
ppMonth :: Year -> Month -> Calendar -> String
ppMonth year month Calendar {events = _events} =
    P.concatMap (\week -> printWeek week year month) weeks ++ printBorder
  where
    weeks = createWeeks year month

-- Prints calander

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
    allEvents = createCalendarDays year month testCalendar

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

createCalendarDays :: Year -> Month -> Calendar -> Map Int [String]
createCalendarDays year' month' Calendar {events = _events} = P.foldr addDayEvent M.empty _events
  where
    addDayEvent event res
      | year (date start) /= year' || month (date start) /= month' = res
      | day (date start) > day (date end) = res -- if start date is after end date
      | day (date start) < day (date end) = res -- Add multible days
      | otherwise = addSingleDayEvent
          
      where
        (start, end) = (getEventStartDateTime event, getEventEndDateTime event)
        addSingleDayEvent = case M.lookup (runDay $ day $ date start) res of
            Nothing -> M.insert (runDay $ day $ date start) [dateTimeToString start ++ " - " ++ dateTimeToString end] res
            Just x -> M.insert (runDay $ day $ date start) ((dateTimeToString start ++ " - " ++ dateTimeToString end) : x) res

    dateTimeToString :: DateTime -> String
    dateTimeToString DateTime {time = Time {hour = _hour, minute = _minute}} = showNum (runHour _hour) ++ ":" ++ showNum (runMinute _minute)
      where
        showNum :: Int -> String
        showNum x
          | x < 9 = "0" ++ show x
          | otherwise = show x

getDayEvents :: Int -> Map Int [String] -> [String]
getDayEvents key map =
  case M.lookup key map of
    Nothing -> []
    Just x -> x
