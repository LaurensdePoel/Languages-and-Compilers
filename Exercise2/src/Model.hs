module Model where

    -- Exercise 1
-- data Token = Token deriving Show
data Token = TArrow | TPoint | TComma | 
            TGo | TTake | TMark | TNone |  
            TCase | TOf | TEnd |
            TTurn | TLeft | TRight | TFront | 
            TSemicolon | TUnderscore | TIdent String | 
            TEmpty | TLambda | TDebris | TAsteroid | TBoundary
             deriving Show


-- Exercise 2
data Program = Program [Rule] deriving Show
-- data Program = Program
--     { rules :: [Rule]
--     }  deriving Show

data Rule = Rule String Cmds 
    deriving Show
data Cmds = EmptyC | Cmds Cmd Cmds 
    deriving Show   
data Cmd = Go | Take | Mark | None | Turn Dir | Case Dir Alts | Ident String
    deriving Show
data Dir = DLeft | DRight | DFront
    deriving Show
data Alts = EmptyA | Alts Alt Alts
    deriving Show
data Alt = Alt Pat Cmds
    deriving Show
data Pat = PEmpty | PLambda | PDebris | PAsteroid | PBoundary | PUnderscore 
    deriving (Show, Eq)