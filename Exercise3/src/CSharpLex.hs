module CSharpLex where

import Data.Char
import Control.Monad (guard)
import ParseLib.Abstract
import Prelude hiding ((<$), (<*), (*>), sequence)
import Data.Maybe

data Token = POpen    | PClose      -- parentheses     ()
           | SOpen    | SClose      -- square brackets []
           | COpen    | CClose      -- curly braces    {}
           | Comma    | Semicolon
           | KeyIf    | KeyElse
           | KeyWhile | KeyReturn
           | KeyTry   | KeyCatch
           | KeyClass | KeyVoid
           | StdType   String       -- the 8 standard types
           | Operator  String       -- the 15 operators
           | UpperId   String       -- uppercase identifiers
           | LowerId   String       -- lowercase identifiers
           | ConstInt  Int
           deriving (Eq, Show)

----- Begin Lexer -----
lexicalScanner :: Parser Char [Token]
lexicalScanner = lexWhiteSpace *> greedy (lexToken <* lexWhiteSpace) <* eof

lexToken :: Parser Char Token
lexToken = greedyChoice
             [ lexTerminal
             , lexEnum StdType stdTypes
             , lexEnum Operator operators
             , lexConstInt
             , lexLowerId
             , lexUpperId
             ]


lexTerminal :: Parser Char Token
lexTerminal = choice [t <$ keyword s | (t,s) <- terminals]
  where
    terminals :: [(Token, String)]
    terminals =
      [ ( POpen     , "("      )
      , ( PClose    , ")"      )
      , ( SOpen     , "["      )
      , ( SClose    , "]"      )
      , ( COpen     , "{"      )
      , ( CClose    , "}"      )
      , ( Comma     , ","      )
      , ( Semicolon , ";"      )
      , ( KeyIf     , "if"     )
      , ( KeyElse   , "else"   )
      , ( KeyWhile  , "while"  )
      , ( KeyReturn , "return" )
      , ( KeyTry    , "try"    )
      , ( KeyCatch  , "catch"  )
      , ( KeyClass  , "class"  )
      , ( KeyVoid   , "void"   )
      ]


lexEnum :: (String -> Token) -> [String] -> Parser Char Token
lexEnum f xs = f <$> choice (map keyword xs)

stdTypes :: [String]
stdTypes = ["int", "long", "double", "float", "byte", "short", "bool", "char"]
operators :: [String]
operators = ["+", "-", "*", "/", "%", "&&", "||", "^", "<=", "<", ">=", ">", "==", "!=", "="]


lexConstInt :: Parser Char Token
lexConstInt = ConstInt . read <$> greedy1 (satisfy isDigit)

lexLowerId :: Parser Char Token
lexLowerId = (\x xs -> LowerId (x:xs)) <$> satisfy isLower <*> greedy (satisfy isAlphaNum)

lexUpperId :: Parser Char Token
lexUpperId = (\x xs -> UpperId (x:xs)) <$> satisfy isUpper <*> greedy (satisfy isAlphaNum)


lexWhiteSpace :: Parser Char String
lexWhiteSpace = greedy (satisfy isSpace)

keyword :: String -> Parser Char String
keyword [] = succeed ""
keyword xs@(x:_) | isLetter x = do ys <- greedy (satisfy isAlphaNum)
                                   guard (xs == ys)
                                   return ys
                 | otherwise  = token xs

greedyChoice :: [Parser s a] -> Parser s a
greedyChoice = foldr (<<|>) empty
----- End Lexer -----


----- Utilities for consuming tokens -----
sStdType :: Parser Token String
sStdType = pFromMaybe fromStdType
  where fromStdType (StdType x) = Just x
        fromStdType _           = Nothing

sUpperId :: Parser Token String
sUpperId = pFromMaybe fromUpperId
    where fromUpperId (UpperId x) = Just x
          fromUpperId _           = Nothing

sLowerId :: Parser Token String
sLowerId = pFromMaybe fromLowerId
  where fromLowerId (LowerId x) = Just x
        fromLowerId _           = Nothing

sConst :: Parser Token Int
sConst  = pFromMaybe fromConst
  where fromConst (ConstInt  x) = Just x
        fromConst _             = Nothing

sOperator :: Parser Token String
sOperator = pFromMaybe fromOperator
  where fromOperator (Operator x) = Just x
        fromOperator _            = Nothing

sSemi :: Parser Token Token
sSemi =  symbol Semicolon


pFromMaybe :: (s -> Maybe a) -> Parser s a
pFromMaybe f = fromJust . f <$> satisfy (isJust . f)
