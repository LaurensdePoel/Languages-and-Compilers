module Interpreter where

import ParseLib.Abstract
import Prelude hiding ((<*), (<$))

import Data.Map (Map)
import qualified Data.Map as L

import Data.Char (isSpace, GeneralCategory (Space))
import Control.Monad (replicateM)

import Lexer
import Parser
import Model
import Algebra


data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary
  deriving Eq

type Size      =  Int
type Pos       =  (Int, Int)
type Space     =  Map Pos Contents


testSpace :: Space
testSpace = L.fromList [
            ((0,0), Empty), ((1,0), Debris), ((2,0), Lambda),
            ((0,1), Empty) , ((1,1), Empty) , ((2,1), Empty),
            ((0,2), Debris), ((1,2), Debris), ((2,2), Debris)
            ]

test = putStrLn (printSpace testSpace)

-- | Parses a space file, such as the ones in the examples folder.
parseSpace :: Parser Char Space
parseSpace = do
    (mr, mc) <- parenthesised ((,) <$> natural <* symbol ',' <*> natural)
                <* spaces
    -- read |mr + 1| rows of |mc + 1| characters
    css      <- replicateM (mr + 1) (replicateM (mc + 1) contents)
    -- convert from a list of lists to a finite map representation
    return $ L.fromList $ concat $
            zipWith (\r cs ->
              zipWith (\c d -> ((r, c), d)) [0..] cs) [0..] css
  where
    spaces :: Parser Char String
    spaces = greedy (satisfy isSpace)

    contents :: Parser Char Contents
    contents = choice (Prelude.map (\(f,c) -> f <$ symbol c) contentsTable)
      <* spaces


-- | Conversion table
contentsTable :: [ (Contents, Char)]
contentsTable =  [ (Empty   , '.' )
                 , (Lambda  , '\\')
                 , (Debris  , '%' )
                 , (Asteroid, 'O' )
                 , (Boundary, '#' )]


-- Exercise 7
printSpace :: Space -> String
printSpace m = printSize ++ printRows 0
    where
      convertContentToChar :: Contents -> Char  
      convertContentToChar x = case lookup x contentsTable of
          Nothing -> error "Content not in contents list"
          Just x -> x

      printSize :: String
      printSize = "(" ++ show maxX ++ "," ++ show maxY ++ ")\n"


      printRow :: Pos -> String
      printRow pos@(x,y) | x <= maxX = case L.lookup pos m of
                                  Nothing -> error "Key not in map"
                                  Just c ->  convertContentToChar c : printRow (x+1,y)
                  | otherwise = "\n"

      printRows :: Int -> String
      printRows y | y <= maxY = printRow (0,y) ++ printRows (y+1)
                  | otherwise = ""

      (maxX, maxY) = fst (L.findMax m)


-- These three should be defined by you
type Ident = String
type Commands = Cmds
type Heading = Direction

data Direction = North | East | South | West
  deriving Show

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack
data Step =  Done  Space Pos Heading
          |  Ok    ArrowState
          |  Fail  String

-- | Exercise 8
toEnvironment :: String -> Environment
toEnvironment xs | checkProgram program = createEnviroment program
                 | otherwise =  error "program not correct"
  where
    program :: Program
    program = Program (parser (alexScanTokens xs))

    createEnviroment :: Program -> Environment
    createEnviroment (Program rules) = L.fromList (map makePair rules)

    makePair :: Rule -> (Ident, Commands)
    makePair (Rule s c) = (s,c)


testEnvironment, testEnvironment2 :: Environment
testEnvironment = toEnvironment "start     -> turn right, go, turn left."
testEnvironment2 = toEnvironment "start    -> go, case front of Boundary -> nothing; _ -> go end."

testArrowState :: ArrowState
testArrowState = ArrowState testSpace (0,0) East testStack

testStack :: Stack
testStack = case L.lookup "start" testEnvironment2 of
      Nothing -> error "Key not found in testStack"
      Just x -> x

-- | Exercise 9
-- putStrLn $ printStep $ step testEnvironment2 testArrowState 
step :: Environment -> ArrowState -> Step
--step env (ArrowState space pos heading stack) = undefined
step env (ArrowState space pos heading EmptyC) = Done space pos heading
step env (ArrowState space pos heading (Cmds cmd cmds)) = handleStack cmd
  where
    doNothing :: Step
    doNothing = Ok (ArrowState space pos heading cmds)

    nextPos :: Pos -> Pos
    nextPos (x,y) = case heading of
          North -> (x, y-1)
          East  -> (x+1, y)
          South -> (x, y+1)
          West  -> (x-1, y)

    handleStack :: Cmd -> Step
    -- go
    handleStack Go = Ok (ArrowState space moveIfPossible heading cmds)
      where
        moveIfPossible :: Pos
        moveIfPossible = case L.lookup (nextPos pos) space of
            Nothing -> pos -- the posistion doesn't exist -> do nothing
            Just x -> if validNextPos x 
              then 
                nextPos pos -- can move to next position
              else 
                pos -- next object not a lambda or a debris
    
        validNextPos :: Contents -> Bool
        validNextPos content = case content of
            Lambda -> True
            Debris ->  True
            _ -> False

    -- take
    handleStack Take = Ok (ArrowState space pos heading cmds)
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
      DLeft   -> Ok (ArrowState space pos (turnLeft heading) cmds)
      DFront  -> doNothing -- turning foreward has no effect
      DRight  -> Ok (ArrowState space pos (turnRight heading) cmds) 

      where
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

    -- case
    handleStack (Case dir stack) = case L.lookup (nextPos pos) space of
        Nothing       -> undefined -- check for Boundary
        Just content  -> undefined -- check with current content
        -- where
        --   contentOf :: Contents
    
    -- rule
    handleStack (Ident ident) = case L.lookup ident env of
      Nothing -> Fail $ "Rule: " ++ ident ++ " is not defined"
      Just stack  -> Ok (ArrowState space pos heading cmds)

printStep :: Step -> String
printStep (Fail mes) = "Fail: " ++ mes
printStep (Ok (ArrowState space pos heading stack)) = 
    "Ok: " ++ printSpace space ++ " Pos:" ++show pos ++ " Heading:" ++ show heading ++ "\nStack: " ++ show stack
printStep (Done space pos heading) = 
    "Done: " ++ printSpace space ++ " Pos:" ++show pos ++ " Heading:" ++ show heading




