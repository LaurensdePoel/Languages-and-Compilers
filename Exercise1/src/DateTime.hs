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
parseDateTime = DateTime <$> parseDate <* (symbol 'T') <*> parseTime <*> parseUtc



parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay
  where
    parseYear = (\x y z q -> Year (x*1000 + y*100 + z *10 + q)) <$> newdigit <*> newdigit <*> newdigit <*> newdigit
    parseMonth = (\x y -> Month (x*10 + y)) <$> newdigit <*> newdigit
    parseDay = (\x y -> Day (x*10 + y)) <$> newdigit <*> newdigit

parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond
  where
    -- parseHour :: Hour
    --parseHour = (\x  -> Hour (read [x] )) <$> digit <* digit
    parseHour = (\x y -> Hour (x*10 + y)) <$> newdigit <*> newdigit
    parseMinute = (\x y -> Minute (x*10 + y)) <$> newdigit <*> newdigit
    parseSecond = (\x y -> Second (x*10 + y)) <$> newdigit <*> newdigit


parseUtc :: Parser Char Bool
parseUtc = const True <$> symbol 'z'
            <|> const False <$> epsilon

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run parser xs | null p = Nothing
              | otherwise = Just (fst (head p))
  where
    p = filter (\(_,s) -> null s) (parse parser xs) 


-- = Just $ fst $ head (filter (\(_,s) -> null s) (parse parser xs) )
  -- case parse parser xs of
  --   ((res,_):_) -> Just res
  --   _ -> Nothing

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime (DateTime (Date (Year y) (Month mon) (Day d)) (Time (Hour h) (Minute min) (Second s)) u) = 
  "year=" ++ show y ++ " month=" ++show mon ++ " day=" ++show d ++ 
  "T" ++ " hour=" ++ show h ++ " minutes=" ++ show min ++ " seconds=" ++ show s ++ " utc=" ++showUTC
    where
       showUTC :: String
       showUTC | u = "Z"
               | otherwise = "NoUTC"

printDate :: Time -> String
printDate (Time (Hour h) (Minute min) (Second s)) = " hour=" ++ show h ++ " minutes=" ++ show min ++ " seconds=" ++ show s

parsePrintDate s = fmap printDate $ run parseTime s


-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime = undefined
