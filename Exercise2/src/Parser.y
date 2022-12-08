{
module Parser where

import Model
}

%name parser
%tokentype { Token }

%token
  "->"      {TArrow}
  "."      {TPoint}
  ","      {TComma}
  "go"      {TGo}
  "take"    {TTake}
  "mark"    {TMark}
  "nothing" {TNone}  
  "turn"    {TTurn}
  "case"    {TCase}  
  "of"      {TOf}
  "end"     {TEnd}  
  "left"    {TLeft}  
  "right"   {TRight}  
  "front"   {TFront}  
  ";"      {TSemicolon}  
  "_"       {TUnderscore}  
  "Lambda"  {TLambda}
  "Debris"  {TDebris}
  "Asteroid" {TAsteroid}
  "Boundary" {TBoundary}
  "Empty"    {TEmpty} -- What to do with this?
  ident    {TIdent $$}
  -- x { Token }

%%

-- Program : Program '.' rule          {$3 : $1}
--         | rule '.'                  { Program [$1] }
--         | {- empty -}               { [] }
        
-- rule : string "->" cmds '.'            {Rule $1 $3}

-- Program :: { [Rule] }

-- exp = [Rule] Rule Cmds Cmd Dir Alts Alt Pat
-- acc = Program Rule Cmds Cmd Dir Alts Alt Pat

-- meant = Program Rule Cmds Cmd Dir Alts Alt Pat

-- recursive 
-- Program : Program rule          {$2 : $1}
--         | rule                 { [$1] }
--         | {- empty -}               { [] }

Program : rule Program              {$1 : $2}
        | rule                      { [$1] }
        | {- empty -}               { [] }
        
rule : ident "->" cmds "."           {Rule $1 $3}

cmds : cmd "," cmds                 {Cmds $1 $3}
     | cmd                          {Cmds $1 EmptyC}
     | {- empty -}                  {EmptyC}

cmd : "go"                          {Go}
    | "take"                        {Take}
    | "mark"                        {Mark}
    | "nothing"                     {None}
    | "turn" dir                    {Turn $2}
    | "case" dir "of" alts "end"    {Case $2 $4}
    | ident                         {Ident $1}

dir : "left"                        {DLeft}
    | "right"                       {DRight}
    | "front"                       {DFront}

alts : alt ";" alts                  {Alts $1 $3}
     | alt                          {Alts $1 EmptyA}
     | {- empty -}                  {EmptyA}

alt : pat "->" cmds                 {Alt $1 $3}

pat : "Empty"                       {PEmpty}
    | "Lambda"                      {PLambda}
    | "Debris"                      {PDebris}
    | "Asteroid"                    {PAsteroid}
    | "Boundary"                    {PBoundary}
    | "_"                           {PUnderscore}

{

happyError _ = error "parse error"

}