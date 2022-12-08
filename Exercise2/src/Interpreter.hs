module Interpreter where

import ParseLib.Abstract
import Prelude hiding ((<*), (<$))

import Data.Map (Map)
import qualified Data.Map as L

import Data.Char (isSpace)
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
            ((0,2), Debris), ((1,2), Debris), ((2,2), Debris),
            ((0,0), Lambda), ((1,0), Lambda), ((2,0), Lambda),
            ((0,1), Empty) , ((1,1), Empty) , ((2,1), Empty)
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
type Commands = ()
type Heading = ()

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step =  Done  Space Pos Heading
          |  Ok    ArrowState
          |  Fail  String

-- | Exercise 8
-- String -> Token (Lexen)
-- Token -> Program (Parsen)
-- Check (Program)  (Check)
-- program -> environment
toEnvironment :: String -> Environment
toEnvironment = undefined

-- | Exercise 9
step :: Environment -> ArrowState -> Step
step = undefined


