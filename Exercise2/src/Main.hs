module Main where

import Algebra
import Control.Arrow (ArrowChoice (left, right))
import Interpreter
import Lexer
import Model
import ParseLib (parse)
import Parser

-- Exercise 11
interactive :: Environment -> ArrowState -> IO ()
interactive env state = do
  takeStep env state
  where
    takeStep :: Environment -> ArrowState -> IO ()
    takeStep env state@(ArrowState _ _ _ st) = do
      print (show st)
      case step env state of
        Done space pos heading -> do
          putStrLn $ printSpace space
          putStrLn ("\nPos:" ++ show pos ++ "\nHeading:" ++ show heading)
        Ok arrow@(ArrowState space pos heading stack@(Cmds cmd cmds)) -> do
          print (show stack)
          -- putStrLn ("Next command: " ++ show cmd)
          putStrLn $ printSpace space
          putStrLn ("\nPos:" ++ show pos ++ "\nHeading:" ++ show heading)
          input <- askInput "Next step (y) or quit (n)\n" ["y", "n"]
          if input == "y" then takeStep env arrow else putStrLn "Stopped current program!\n"
        Fail messages -> error messages

-- interactive :: Environment -> ArrowState -> IO ()
-- interactive env state = do
--   case step env state of
--     Done space pos heading -> do
--       putStrLn $ printSpace space
--       putStrLn ("\nPos:" ++show pos ++ "\nHeading:" ++ show heading)
--     Ok arrow@(ArrowState space pos heading stack) -> do
--       putStrLn $ printSpace space
--       putStrLn ("\nPos:" ++show pos ++ "\nHeading:" ++ show heading)
--       putStrLn "Make a move:\n-1 go\n-2 turn left\n-3 turn right\n-4 mark\n-5 take\n- none\n-7 end\n"
--       val <- getLine
--       if (validInput val ["1","2","3","4","5","6","7"]) then
--         interactive env (ArrowState space pos heading (Cmds Go stack))
--       else
--         putStrLn "invalid input"
--     Fail messages -> putStrLn messages

batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch = takeStep
  where
    takeStep env state =
      case step env state of
        Done space pos heading -> (space, pos, heading)
        Ok arrowState -> takeStep env arrowState
        Fail messages -> error messages

-- This function is just here to play around with and test your lexer/parser.
-- When implementing exercise 11, delete this comment and this function,
-- and write a new main function.
main :: IO ()
main = do
  putStrLn "Welcome to the Arrow program\nPlease enter the path of '.space' file you want to load (e.g. ./examples/SampleSpace.space).\nPath:"
  -- spaceFilePath <- getLine
  spaceFile <- readFile "./examples/SampleSpace.space" -- ("./examples/" ++ spaceFilePath)
  putStrLn "Please enter '.arrow' file path (e.g. ./examples/Add.arrow).\nPath:"
  pathEnv <- readFile "./examples/RemoveDebris.arrow"
  mode <- askInput "Select the preferred mode?\n- 1. Interactive\n- 2. Batch\n" ["1", "2"]
  let newSpace = run parseSpace spaceFile
  -- putStrLn (printSpace newSpace)
  let env = toEnvironment pathEnv
  if mode == "1"
    then -- then interactive env arrowState

    -- putStrLn "Not inplemented"
    -- interactive testE (ArrowState newSpace (0,0) East (loadStack testE))
      interactive env (ArrowState newSpace (0, 0) East (loadStack env))
    else -- restart programm
    do
      -- putStrLn "Please enter '.arrow' file path"
      -- arrowFile <- getLine
      -- let env = toEnvironment ("./examples/" ++ arrowFile)
      -- pathEnv <- readFile "./examples/Add.arrow"
      -- let env = toEnvironment pathEnv

      let (sp, pos, heading) = batch env (ArrowState newSpace (0, 0) East (loadStack env))
      putStrLn $ printSpace sp
      putStrLn ("\nPos:" ++ show pos ++ "\nHeading:" ++ show heading)

  input <- askInput "Run program with other files (y) or quit program (n)\n" ["y", "n"]
  if input == "y" then main else putStrLn "\nMain is done"

-- chars <- readFile "examples/Add.arrow"
-- putStrLn "Input program:"
-- putStrLn ""
-- putStrLn chars
-- putStrLn ""
-- let tokens = alexScanTokens chars
-- putStrLn "Tokens:"
-- putStrLn ""
-- print tokens
-- let arr = parser tokens
-- putStrLn ""
-- putStrLn "Parsed program:"
-- putStrLn ""
-- putStrLn "\nMain is done"

validInput :: String -> [String] -> Bool
validInput input = any ((== True) . (input ==))

askInput :: String -> [String] -> IO String
askInput question validValues = do
  putStrLn question
  val <- getLine
  if (validInput val validValues)
    then return val
    else do
      putStrLn "Invalid input please enter correct input."
      askInput question validValues

-- return ""

-- 1. go
-- 2. case
-- 1. left
-- 2. front
-- 3. right
--