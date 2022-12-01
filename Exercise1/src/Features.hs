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

-- getEventDateTimes :: Event -> (DateTime, DateTime)
-- getEventDateTimes Event {eventprops = _eventprops} = P.foldr isDate (,) _eventprops
--   where
--     isDate (DTSTART x) (_, e) = (x, e)
--     isDate (DTEND x) (s, _) = (s, x)
--     isDate _ res = res

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
ppMonth year month calendar = render $ createDayBox "1 1 12:00-13:00 14:00-15:00"
  where
    -- createMonth = columns left 5 5 "test kaljfklj akdjf\ntest2" -- nrDaysOfMonth (runYear year) (runMonth month)
    fixedWidth :: Int
    fixedWidth = 20
    boxBorder :: String
    boxBorder = replicate fixedWidth '-'
    columnBorder :: [Int] -> String
    columnBorder boxHeights = (concatMap (\height -> "+" ++ replicate height '|') boxHeights) ++ "+"
    createDayBox :: String -> Box
    createDayBox dayInfo = para top fixedWidth (boxBorder ++ boxInfo ++ boxBorder)
      where
        elements = words dayInfo
        boxInfo = concatMap (\element -> element ++ replicate (fixedWidth - length element) ' ') elements

ppMyBox :: Int -> Int -> String -> String
ppMyBox sizeX sizeY message = row sizeX ++ fillfield message
  where
    row sizeX = "+" ++ replicate (sizeX - 2) '-' ++ "+\n"
    fillfield fieldVal = fieldVal ++ replicate (length fieldVal - sizeX) ' '

-- get a [Box] that contains all days

-- create week = [Box] that are glued together with hcat

-- create a month by putting the weeks together

printBorder :: String
printBorder = concat (replicate 7 ("+" ++ replicate 13 '-')) ++ "+" ++ "\n"

-- * Exercise 3

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

printWeek :: [Int] -> String
printWeek xs = printBorder ++ printHeader xs ++ printWeekEvents xs

printWeekEvents :: [Int] -> String
printWeekEvents xs = concatMap printRow (transpose (convertWeekToMaxSize (getEventsInWeek xs)))
  where
    allEvents = createCalendarDays tmpYear tmpMonth testCalendar

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

printTable :: Calendar -> String
printTable Calendar {events = _events} =
  printWeek week1
    ++ printWeek week2
    ++ printWeek week3
    ++ printWeek week4
    ++ printWeek week5
    ++ printBorder
  where
    allDays = createDaysheading (Year 2000) (Month 7) -- Dates tmp
    -- creates weeks
    week1 = P.take 7 allDays
    week2 = P.take 7 $ P.drop 7 allDays
    week3 = P.take 7 $ P.drop 14 allDays
    week4 = P.take 7 $ P.drop 21 allDays
    week5
      | length tmpWeek5 < 7 = tmpWeek5 ++ P.take (7 - length tmpWeek5) [0, 0 ..]
      | otherwise = P.drop 29 allDays
    tmpWeek5 = P.drop 29 allDays

tmpYear = Year 1997

tmpMonth = Month 7

createDaysheading :: Year -> Month -> [Int]
createDaysheading year month = getAllDays
  where
    getAllDays = P.take amountOfDays [1, 2 ..]
    amountOfDays = nrDaysOfMonth year month

createCalendarDays :: Year -> Month -> Calendar -> Map Int [String]
createCalendarDays year' month' Calendar {events = _events} = P.foldr bla M.empty _events
  where
    bla event res
      -- \| year $ date _dtstart /= year' && month $ date _dtstart /= month' = res
      | year (date start) /= year' || month (date start) /= month' = res
      | otherwise =
          case M.lookup (runDay $ day $ date start) res of
            Nothing -> M.insert (runDay $ day $ date start) [dateTimeToString start ++ " - " ++ dateTimeToString end] res
            Just x -> M.insert (runDay $ day $ date start) ((dateTimeToString start ++ " - " ++ dateTimeToString end) : x) res
      where
        (start, end) = (getEventStartDateTime event, getEventEndDateTime event)

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

-- logica samenvoegen

-- printTable table@(header : rows) = [printLine width] ++ header' ++ [printLine width] ++ rows' ++ [printLine width]
--   where
--     width = columnWidths table
--     headerU = map (map toUpper) header
--     header' = map printRow [zip width headerU]
--     rows' = concatMap (\row -> map printRow [zip width row]) rows