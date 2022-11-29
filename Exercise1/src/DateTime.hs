module DateTime where

import Data.Time.Calendar (gregorianMonthLength)
import ParseLib.Abstract
import Prelude hiding (sequence, ($>), (*>), (<$), (<*))

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime
  { date :: Date,
    time :: Time,
    utc :: Bool
  }
  deriving (Eq, Ord, Show)

data Date = Date
  { year :: Year,
    month :: Month,
    day :: Day
  }
  deriving (Eq, Ord, Show)

newtype Year = Year {runYear :: Int} deriving (Eq, Ord, Show)

newtype Month = Month {runMonth :: Int} deriving (Eq, Ord, Show)

newtype Day = Day {runDay :: Int} deriving (Eq, Ord, Show)

data Time = Time
  { hour :: Hour,
    minute :: Minute,
    second :: Second
  }
  deriving (Eq, Ord, Show)

newtype Hour = Hour {runHour :: Int} deriving (Eq, Ord, Show)

newtype Minute = Minute {runMinute :: Int} deriving (Eq, Ord, Show)

newtype Second = Second {runSecond :: Int} deriving (Eq, Ord, Show)

-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <* (symbol 'T') <*> parseTime <*> parseUtc

parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay
  where
    parseYear = (\x y z q -> Year (x * 1000 + y * 100 + z * 10 + q)) <$> newdigit <*> newdigit <*> newdigit <*> newdigit
    parseMonth = (\x y -> Month (x * 10 + y)) <$> newdigit <*> newdigit
    parseDay = (\x y -> Day (x * 10 + y)) <$> newdigit <*> newdigit

parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond
  where
    -- parseHour :: Hour
    -- parseHour = (\x  -> Hour (read [x] )) <$> digit <* digit
    parseHour = (\x y -> Hour (x * 10 + y)) <$> newdigit <*> newdigit
    parseMinute = (\x y -> Minute (x * 10 + y)) <$> newdigit <*> newdigit
    parseSecond = (\x y -> Second (x * 10 + y)) <$> newdigit <*> newdigit

parseUtc :: Parser Char Bool
parseUtc =
  const True <$> symbol 'Z'
    <|> const False <$> epsilon

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run parser xs
  | null p = Nothing
  | otherwise = Just (fst (head p))
  where
    p = filter (\(_, s) -> null s) (parse parser xs)

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime (DateTime (Date (Year y) (Month mon) (Day d)) (Time (Hour h) (Minute min) (Second s)) u) =
  showNum y
    ++ showNum mon
    ++ showNum d
    ++ "T"
    ++ showNum h
    ++ showNum min
    ++ showNum s
    ++ showUTC
  where
    showUTC :: String
    showUTC
      | u = "Z"
      | otherwise = ""

    showNum :: Int -> String
    showNum x
      | x < 10 = "0" ++ show x
      | otherwise = show x

-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime (DateTime date time _) = isValidDate date && isValidTime time
  where
    isValidTime :: Time -> Bool
    isValidTime (Time (Hour h) (Minute m) (Second s)) = minMax h (0, 23) && minMax m (0, 59) && minMax s (0, 59)

    isValidDate :: Date -> Bool
    isValidDate (Date (Year y) (Month m) (Day d)) =
      minMax (abs y) (0, 9999)
        && minMax m (1, 12)
        && minMax d (1, numDays)
      where
        numDays :: Int
        numDays = gregorianMonthLength (fromIntegral y) m

    minMax :: Int -> (Int, Int) -> Bool
    minMax x (min, max) = x >= min && x <= max
