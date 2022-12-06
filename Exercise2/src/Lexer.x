{
module Lexer where

import Model
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-
  $white+     ;
  "--".*      ;
  "->"        {\_ -> TArrow}
  "\."        {\_ -> TPoint}
  "\,"        {\_ -> TComma}
  "go"        {\_ -> TGo}
  "take"      {\_ -> TTake}
  "mark"      {\_ -> TMark}
  "nothing"   {\_ -> TNothing}
  "turn"      {\_ -> TTurn}
  "case"      {\_ -> TCase}
  "of"        {\_ -> TOf}
  "end"       {\_ -> TEnd}
  "left"      {\_ -> TLeft}
  "right"     {\_ -> TRight}
  "front"     {\_ -> TFront}
  "\;"        {\_ -> TSemicolon}
  [$alpha $digit \- \+]*  {\s -> TIdent s}

