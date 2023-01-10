module CSharpCode where

import Prelude hiding (LT, GT, EQ)
import qualified Data.Map as M

import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM

{-
  This file contains a starting point for the code generation.
-}

-- The types that we generate for each datatype: our type variables for the algebra.
-- Change these definitions instead of the function signatures to get better type errors.
type C = Code                   -- Class
type M = Code                   -- Member
type S = Code                   -- Statement
type E = ValueOrAddress -> Code -- Expression


codeAlgebra :: CSharpAlgebra C M S E
codeAlgebra = CSharpAlgebra
  fClass
  fMembDecl
  fMembMeth
  fStatDecl
  fStatExpr
  fStatIf
  fStatWhile
  fStatReturn
  fStatBlock
  fExprCon
  fExprVar
  fExprOp



fClass :: String -> [M] -> C
fClass c ms = [Bsr "main", HALT] ++ concat ms



fMembDecl :: Decl -> M
fMembDecl d = []

fMembMeth :: Type -> String -> [Decl] -> S -> M
fMembMeth t x ps s = [LABEL x] ++ s ++ [RET]



fStatDecl :: Decl -> S
fStatDecl d = []

fStatExpr :: E -> S
fStatExpr e = e Value ++ [pop]

fStatIf :: E -> S -> S -> S
fStatIf e s1 s2 = c ++ [BRF (n1 + 2)] ++ s1 ++ [BRA n2] ++ s2
    where
        c        = e Value
        (n1, n2) = (codeSize s1, codeSize s2)

fStatWhile :: E -> S -> S
fStatWhile e s1 = [BRA n] ++ s1 ++ c ++ [BRT (-(n + k + 2))]
    where
        c = e Value
        (n, k) = (codeSize s1, codeSize c)

fStatReturn :: E -> S
fStatReturn e = e Value ++ [pop] ++ [RET]

fStatBlock :: [S] -> S
fStatBlock = concat



fExprCon :: Int -> E
fExprCon n va = [LDC n]

fExprVar :: String -> E
fExprVar x va = let loc = 42 in case va of
                              Value    ->  [LDL  loc]
                              Address  ->  [LDLA loc]

fExprOp :: String -> E -> E -> E
fExprOp "=" e1 e2 va = e2 Value ++ [LDS 0] ++ e1 Address ++ [STA 0]
fExprOp op  e1 e2 va = e1 Value ++ e2 Value ++ [opCodes M.! op]
  where
    opCodes :: M.Map String Instr
    opCodes = M.fromList [ ("+", ADD), ("-",  SUB), ("*", MUL), ("/", DIV), ("%", MOD)
                          , ("<=", LE), (">=",  GE), ("<",  LT), (">",  GT), ("==", EQ)
                          , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                          ]


-- | Whether we are computing the value of a variable, or a pointer to it
data ValueOrAddress = Value | Address
  deriving Show
