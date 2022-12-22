module Main where

import Algebra
import Control.Arrow (ArrowChoice (left, right))
import Interpreter
import Lexer
import Model
import ParseLib (parse)
import Parser

-- Exercise 11
-- This function prints all steps of the program
-- After each step it asks for user input,
-- to either invoke the next step or stop iterating over the program.
interactive :: Environment -> ArrowState -> IO ()
interactive env state = do
  takeStep env state
  where
    takeStep :: Environment -> ArrowState -> IO ()
    takeStep env state = do
      case step env state of
        Done space pos heading -> do printInfo space pos heading
        Ok arrow@(ArrowState space pos heading stack) -> do
          printInfo space pos heading
          putStrLn "Next command:"
          case stack of
            (Cmds cmd _) -> print $ show cmd
            (EmptyC) -> putStrLn "No commands stack is empty"
          input <- askInput "\nNext step (y) or quit (n)\n" ["y", "n"]
          if input == "y" then takeStep env arrow else putStrLn "Stopped current program!\n"
        Fail messages -> error messages

-- This function runs the program
-- and prints the final state of the program
batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch = takeStep
  where
    takeStep env state =
      case step env state of
        Done space pos heading -> (space, pos, heading)
        Ok arrowState -> takeStep env arrowState
        Fail messages -> error messages

-- Prints the current state and additional info of the current state
printInfo :: Space -> Pos -> Heading -> IO ()
printInfo space pos heading = do
  putStrLn "---------------------------------------"
  putStrLn $ printSpace space
  putStrLn ("Current position:" ++ show pos ++ "\nHeading: " ++ show heading)
  putStrLn "---------------------------------------"

-- This function ask for a ".space" and ".arrow" as input
-- and computes the program in the specified states,
-- which is also asked (user input).
main :: IO ()
main = do
  -- Ask for the space file
  putStrLn "Welcome to the Arrow program\nPlease enter the path of '.space' file you want to load (e.g. :\n./examples/SampleSpace.space or\n./examples/AddInput.space).\nPath:"
  spaceFilePath <- getLine
  spaceFile <- readFile spaceFilePath -- readFile "./examples/SampleSpace.space" -- ("./examples/" ++ spaceFilePath)

  -- Ask for the arrow file
  putStrLn "Please enter '.arrow' file path (e.g. :\n./examples/Add.arrow or\n./examples/RemoveDebris.arrow).\nPath:"
  pathEnv <- getLine -- readFile "./examples/RemoveDebris.arrow"
  arrowFile <- readFile pathEnv

  -- Ask for the start direction (Heading)
  direction <- askInput "Select the preferred starting heading:\n- 1. North\n- 2. East\n- 3. South\n- 4. West" ["1", "2", "3", "4"]
  let startDirection = case direction of
        "1" -> North
        "2" -> East
        "3" -> South
        _ -> West

  -- run interactive or the batch mode
  mode <- askInput "Select the preferred mode?\n- 1. Interactive\n- 2. Batch\n" ["1", "2"]
  let newSpace = run parseSpace spaceFile
  let env = toEnvironment arrowFile
  if mode == "1"
    then interactive env (ArrowState newSpace (0, 0) startDirection (loadStack env))
    else do
      let (space, pos, heading) = batch env (ArrowState newSpace (0, 0) startDirection (loadStack env))
      do printInfo space pos heading

  input <- askInput "Run program with other files (y) or quit program (n)\n" ["y", "n"]
  if input == "y" then main else putStrLn "\nMain is done"

-- This function handles questions,
-- returns the answer if it is valid,
-- otherwise ask the question again.
askInput :: String -> [String] -> IO String
askInput question validValues = do
  putStrLn question
  val <- getLine
  if val `elem` validValues
    then return val
    else do
      putStrLn "Invalid input please enter correct input."
      askInput question validValues