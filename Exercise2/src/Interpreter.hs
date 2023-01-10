module Interpreter where

import Algebra
import Control.Monad (replicateM)
import Data.Char (GeneralCategory (ParagraphSeparator, Space), isSpace)
import Data.Map (Map)
import qualified Data.Map as L
import Lexer
import Model
import ParseLib.Abstract
import Parser
import Type.Reflection (SomeTypeRep (SomeTypeRep))
import Prelude hiding ((<$), (<*))

data Contents = Empty | Lambda | Debris | Asteroid | Boundary
  deriving (Eq)

type Size = Int

type Pos = (Int, Int)

type Space = Map Pos Contents

-- | Parses a space file, such as the ones in the examples folder.
parseSpace :: Parser Char Space
parseSpace = do
  (mr, mc) <-
    parenthesised ((,) <$> natural <* symbol ',' <*> natural)
      <* spaces
  -- read |mr + 1| rows of |mc + 1| characters
  css <- replicateM (mr + 1) (replicateM (mc + 1) contents)
  -- convert from a list of lists to a finite map representation
  return $
    L.fromList $
      concat $
        zipWith
          ( \r cs ->
              zipWith (\c d -> ((r, c), d)) [0 ..] cs
          )
          [0 ..]
          css
  where
    spaces :: Parser Char String
    spaces = greedy (satisfy isSpace)

    contents :: Parser Char Contents
    contents =
      choice (Prelude.map (\(f, c) -> f <$ symbol c) contentsTable)
        <* spaces

-- | Conversion table
contentsTable :: [(Contents, Char)]
contentsTable =
  [ (Empty, '.'),
    (Lambda, '\\'),
    (Debris, '%'),
    (Asteroid, 'O'),
    (Boundary, '#')
  ]

-- Exercise 7
-- This function converts a Space into a string, which represent the space 
-- in a text format, which can be printed.
printSpace :: Space -> String
printSpace m = printSize ++ printRows 0
  where
    convertContentToChar :: Contents -> Char
    convertContentToChar x = case lookup x contentsTable of
      Nothing -> error "Content not in contents list"
      Just x -> x

    printSize :: String
    printSize = "(" ++ show maxY ++ "," ++ show maxX ++ ")\n"

    printRow :: Pos -> String
    printRow pos@(y, x)
      | x <= maxX = case L.lookup pos m of
        Nothing -> error "Key not in map"
        Just c -> convertContentToChar c : printRow (y, x + 1)
      | otherwise = "\n"

    printRows :: Int -> String
    printRows y
      | y <= maxY = printRow (y, 0) ++ printRows (y + 1)
      | otherwise = ""

    (maxY, maxX) = fst (L.findMax m)


type Ident = String

type Commands = Cmds

type Heading = Direction

data Direction = North | East | South | West
  deriving (Show)

type Environment = Map Ident Commands

type Stack = Commands

data ArrowState = ArrowState Space Pos Heading Stack

data Step
  = Done Space Pos Heading
  | Ok ArrowState
  | Fail String

-- | Exercise 8
-- This function converts a string to an Environment data type.
-- It Parses the string to a program, validates the program
-- (gives an error if this is not the case),
-- and converts the program into an Environment.
toEnvironment :: String -> Environment
toEnvironment xs
  | checkProgram program = createEnviroment program
  | otherwise = error "program not correct"
  where
    program :: Program
    program = Program (parser (alexScanTokens xs))

    createEnviroment :: Program -> Environment
    createEnviroment (Program rules) = L.fromList (map makePair rules)

    makePair :: Rule -> (Ident, Commands)
    makePair (Rule s c) = (s, c)

-- | Exercise 9
-- The following command can be used to test the step function:
-- ghci> putStrLn $ printStep $ step testEnvironment2 testArrowState

-----------------------------------------------------------------
-- The following variables are defined to test the step function,
-- and/or related functions

testSpace :: Space
testSpace =
  L.fromList
    [ ((0, 0), Empty),
      ((1, 0), Empty),
      ((2, 0), Empty),
      ((0, 1), Empty),
      ((1, 1), Empty),
      ((2, 1), Empty),
      ((0, 2), Empty),
      ((1, 2), Empty),
      ((2, 2), Empty)
    ]

testEnvironment, testEnvironment2 :: Environment
testEnvironment = toEnvironment "start     -> take."
testEnvironment2 = toEnvironment "start    -> case left of Boundary -> turn right; Lambda -> go, go, go; _ -> go end."

testArrowState :: ArrowState
testArrowState = ArrowState testSpace (2, 2) North (loadStack testEnvironment)

-- This function converts an Environment into a Stack
loadStack :: Environment -> Stack
loadStack env = case L.lookup "start" env of
  Nothing -> error "Key not found in testStack"
  Just x -> x

-----------------------------------------------------------------

-- This function executes this first command (on the stack) of the program.
-- Updates the space, position, heading and the stack of commands.
step :: Environment -> ArrowState -> Step
step env (ArrowState space pos heading EmptyC) = Done space pos heading
step env (ArrowState space pos heading (Cmds cmd cmds)) = handleStack cmd
  where
    doNothing :: Step
    doNothing = Ok (ArrowState space pos heading cmds)

    nextPos :: Pos -> Heading -> Pos
    nextPos (y, x) heading' = case heading' of
      North -> (y - 1, x)
      East -> (y, x + 1)
      South -> (y + 1, x)
      West -> (y, x - 1)

    turnLeft, turnRight :: Heading -> Heading
    turnLeft heading = case heading of
      North -> West
      West -> South
      South -> East
      East -> North
    turnRight heading = case heading of
      North -> East
      East -> South
      South -> West
      West -> North

    mergeStack :: Cmds -> Cmds -> Cmds
    mergeStack EmptyC oldStack = oldStack
    mergeStack (Cmds cmd' cmds') oldStack = Cmds cmd' (mergeStack cmds' oldStack)

    handleStack :: Cmd -> Step
    -- go
    handleStack Go = Ok (ArrowState space moveIfPossible heading cmds)
      where
        moveIfPossible :: Pos
        moveIfPossible = case L.lookup (nextPos pos heading) space of
          Nothing -> pos -- the posistion doesn't exist -> do nothing
          Just x ->
            if validNextPos x
              then nextPos pos heading -- can move to next position
              else -- undefined -- next object not a lambda or a debris
                pos -- next object not a lambda or a debris
        validNextPos :: Contents -> Bool
        validNextPos content = case content of
          Lambda -> True
          Debris -> True
          Empty -> True
          _ -> False

    -- take
    handleStack Take = Ok (ArrowState (takeContent space) pos heading cmds)
      where
        takeContent :: Space -> Space
        takeContent = L.insert pos Empty

    -- mark
    handleStack Mark = Ok (ArrowState (placeMark space) pos heading cmds)
      where
        placeMark :: Space -> Space
        placeMark = L.insert pos Lambda

    -- nothing
    handleStack None = doNothing
    -- turn
    handleStack (Turn dir) = case dir of
      DLeft -> Ok (ArrowState space pos (turnLeft heading) cmds)
      DFront -> doNothing -- turning foreward has no effect
      DRight -> Ok (ArrowState space pos (turnRight heading) cmds)
    -- case
    handleStack (Case dir alts) = case dir of
      DLeft -> getNewStack (nextPos pos (turnLeft heading))
      DFront -> getNewStack (nextPos pos heading)
      DRight -> getNewStack (nextPos pos (turnRight heading))
      where
        getNewStack pos' = case L.lookup pos' space of
          Nothing -> handleAlts alts Boundary -- location doesn't exist so it is a boundery and check with Boundary
          Just content -> handleAlts alts content -- check with current content
        handleAlts :: Alts -> Contents -> Step
        handleAlts EmptyA content = Fail "There are not options defined in the Case statement"
        handleAlts (Alts (Alt pat newCmds) alts) content
          | patAndContentIsEqual = Ok (ArrowState space pos heading (mergeStack newCmds cmds)) -- corosponding al
          | otherwise = handleAlts alts content -- check next alt in the Alts list
          where
            patAndContentIsEqual =
              pat == PEmpty && content == Empty
                || pat == PLambda && content == Lambda
                || pat == PDebris && content == Debris
                || pat == PAsteroid && content == Asteroid
                || pat == PBoundary && content == Boundary
                || pat == PUnderscore

    -- rule
    handleStack (Ident ident) = case L.lookup ident env of
      Nothing -> Fail $ "Rule: " ++ ident ++ " is not defined"
      Just stack -> Ok (ArrowState space pos heading (mergeStack stack cmds))

printStep :: Step -> String
printStep (Fail mes) = "Fail: " ++ mes
printStep (Ok (ArrowState space pos heading stack)) =
  "Ok: " ++ printSpace space ++ "\nPos:" ++ show pos ++ "\nHeading:" ++ show heading ++ "\nStack: " ++ show stack
printStep (Done space pos heading) =
  "Done: " ++ printSpace space ++ "\nPos:" ++ show pos ++ "\nHeading:" ++ show heading

-- This function is used to parse a string with a given parser.
run :: Parser a b -> [a] -> b
run parser xs
  | null p = error "not correct"
  | otherwise = fst (head p)
  where
    p = filter (\(_, s) -> null s) (parse parser xs)
