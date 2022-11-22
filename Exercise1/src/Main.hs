-- | Used for IO, do not edit.
module Main where

import DateTime
import Calendar
import Features
import System.Environment
import System.IO


data Result = SyntaxError | Invalid DateTime | Valid DateTime deriving (Eq, Ord)

instance Show DateTime where
    show = printDateTime

instance Show Result where
    show SyntaxError = "date/time with wrong syntax"
    show (Invalid _) = "good syntax, but invalid date or time values"
    show (Valid x)   = "valid date: " ++ show x

main :: IO ()
main = do
  setNewlineTranslations
  mainDateTime

mainDateTime :: IO ()
mainDateTime = interact (printOutput . processCheck . processInput)
    where
        processInput = map (run parseDateTime) . lines
        processCheck = map (maybe SyntaxError (\x -> if checkDateTime x then Valid x else Invalid x))
        printOutput  = unlines . map show

mainCalendar :: IO ()
mainCalendar = do
    file:_ <- getArgs
    res <- readCalendar file
    putStrLn $ maybe "Calendar parsing error" (ppMonth (Year 2012) (Month 11)) res

readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar path = do
  setNewlineTranslations
  string <- readFile path
  return $ recognizeCalendar string

setNewlineTranslations :: IO ()
setNewlineTranslations = do
  hSetNewlineMode stdin  noNewlineTranslation
  hSetNewlineMode stdout noNewlineTranslation
