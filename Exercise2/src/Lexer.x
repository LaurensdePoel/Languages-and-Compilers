{
module Lexer where

import Model
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters
$ls_ds = [a-zA-Z0-9]           -- letters and digits

tokens :-

  $white+     ;
  "--".*      ;
  "->"        {const TArrow}
  "."        {const TPoint}
  ","        {const TComma}
  "go"        {const TGo}
  "take"      {const TTake}
  "mark"      {const TMark}
  "nothing"   {const TNone}
  "turn"      {const TTurn}
  "case"      {const TCase}
  "of"        {const TOf}
  "end"       {const TEnd}
  "left"      {const TLeft}
  "right"     {const TRight}
  "front"     {const TFront}
  ";"        {const TSemicolon}
  "Empty"     {const TEmpty} --???
  "Lambda"       {const TLambda} --???
  "Debris"      {const TDebris} --???
  "Asteroid"    {const TAsteroid} --???
  "Boundary"    {const TBoundary} --???
  "_"         {const TUnderscore}
  $alpha [$alpha $digit \+ \-]+     {\s -> TIdent s}
  -- .*          ;

