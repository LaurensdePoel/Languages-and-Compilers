module DateTime where

import ParseLib.Abstract
import Prelude hiding (sequence, ($>), (*>), (<$), (<*))

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime
  { date :: Date,
    time :: Time,
    utc :: Bool
  }
  deriving (Eq, Ord)

data Date = Date
  { year :: Year,
    month :: Month,
    day :: Day
  }
  deriving (Eq, Ord)

newtype Year = Year {runYear :: Int} deriving (Eq, Ord)

newtype Month = Month {runMonth :: Int} deriving (Eq, Ord)

newtype Day = Day {runDay :: Int} deriving (Eq, Ord)

data Time = Time
  { hour :: Hour,
    minute :: Minute,
    second :: Second
  }
  deriving (Eq, Ord)

newtype Hour = Hour {runHour :: Int} deriving (Eq, Ord)

newtype Minute = Minute {runMinute :: Int} deriving (Eq, Ord)

newtype Second = Second {runSecond :: Int} deriving (Eq, Ord)

-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <*> parseTime <*> parseUtc

parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay
  where
    parseYear = Year <$> integer
    parseMonth = Month <$> integer
    parseDay = Day <$> integer

parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond
  where
    parseHour = Hour <$> integer
    parseMinute = Minute <$> integer
    parseSecond = Second <$> integer

parseUtc :: Parser Char Bool
parseUtc = const True <$> symbol 'z'

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run = undefined

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime = undefined

-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime = undefined
