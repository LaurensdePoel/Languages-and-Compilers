module Model where

-- Exercise 1
-- The Token datatype represents tokens which are returned by the lexer/scanner.
data Token
  = TArrow
  | TPoint
  | TComma
  | TGo
  | TTake
  | TMark
  | TNone
  | TCase
  | TOf
  | TEnd
  | TTurn
  | TLeft
  | TRight
  | TFront
  | TSemicolon
  | TIdent String
  | TEmpty
  | TLambda
  | TDebris
  | TAsteroid
  | TBoundary
  | TUnderscore
  deriving (Show)

-- Exercise 2
-- Abstract syntax which describes the arrow language
-- The Program datatype is the starting nonterminal
data Program = Program [Rule] deriving (Show)

data Rule = Rule String Cmds
  deriving (Show)

data Cmds = EmptyC | Cmds Cmd Cmds
  deriving (Show)

data Cmd = Go | Take | Mark | None | Turn Dir | Case Dir Alts | Ident String
  deriving (Show)

data Dir = DLeft | DRight | DFront
  deriving (Show)

data Alts = EmptyA | Alts Alt Alts
  deriving (Show)

data Alt = Alt Pat Cmds
  deriving (Show)

data Pat = PEmpty | PLambda | PDebris | PAsteroid | PBoundary | PUnderscore
  deriving (Show, Eq)