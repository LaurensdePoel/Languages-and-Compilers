module Model where

-- Exercise 1
data Token = TArrow | TPoint | TComma | TGo | TTake | TMark | TNothing | TTurn | TCase | TOf | TEnd |
             TLeft | TRight | TFront | TSemicolon | TIdent String
             deriving Show


-- Exercise 2
data Program = Program deriving Show
