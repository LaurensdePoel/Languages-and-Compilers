module Algebra where

import Model

-- Exercise 5

-- lecture notes page 86

-- type PAlgebra p = (p -> p)               -- Program
-- type RAlgebra r = (String -> r -> r)     -- rule
-- type CsAlgebra cs = (cs, cs -> cs)       -- cmds
-- type CAlgebra c = (c, c -> c, c -> c -> c, String -> c) -- cmd
-- type DAlgebra d = d                         -- dir
-- type ASAlgebra as = (as, as -> as -> as)    -- alts
-- type AAlgebra a = (a -> a -> a)             -- alt
-- type PTAlgebra pt = pt                    -- pat

type TESTAlgebra pro rule cmds cmd dir alts alt pat =
  ( ([rule] -> pro), -- Program
    (String -> cmds -> rule),
    (cmds, cmd -> cmds -> cmds),
    (cmd, dir -> cmd, dir -> alts -> cmd, String -> cmd),
    (dir),
    (alts, alt -> alts -> alts),
    (pat -> cmds -> alt),
    (pat, pat, pat, pat, pat, pat)
  )

fold :: TESTAlgebra p r cs c d as a pt -> Program -> p
fold ((p1), (r1), (cs1, cs2), (c1, c2, c3, c4), (d1), (as1, as2), (a1), (pt1, pt2, pt3, pt4, pt5, pt6)) = fold'
  where
    fold' (Program xs) = p1 (map foldR xs)
    foldR (Rule name cmds) = r1 name (foldCs cmds)
    foldCs (EmptyC) = cs1
    foldCs (Cmds cmd cmds) = cs2 (foldC cmd) (foldCs cmds)
    foldC (Go) = c1
    foldC (Take) = c1
    foldC (Mark) = c1
    foldC (None) = c1
    foldC (Turn dir) = c2 (foldD dir)
    foldC (Case dir alts) = c3 (foldD dir) (foldAs alts)
    foldC (Ident ident) = c4 ident
    foldD (DLeft) = d1
    foldD (DRight) = d1
    foldD (DFront) = d1
    foldAs (EmptyA) = as1
    foldAs (Alts alt alts) = as2 (foldA alt) (foldAs alts)
    foldA (Alt pat cmds) = a1 (foldPt pat) (foldCs cmds)
    foldPt (PEmpty) = pt1
    foldPt (PLambda) = pt2
    foldPt (PDebris) = pt3
    foldPt (PAsteroid) = pt4
    foldPt (PBoundary) = pt5
    foldPt (PUnderscore) = pt6

-- Exercise 6
-- ghci> checkProgram2 (Program (parser (alexScanTokens "test -> go.")))
-- False
-- ghci> checkProgram2 (Program (parser (alexScanTokens "start -> go.")))
-- True
-- ghci> checkProgram2 (Program (parser (alexScanTokens "start -> go, markisgay.")))
-- False
-- ghci> checkProgram2 (Program (parser (alexScanTokens "start -> go, markisgay.markisgay -> go.")))
-- True
-- ghci> checkProgram2 (Program (parser (alexScanTokens "start -> go, markisgay.markisgay -> go.markisgay -> go.")))
-- False

checkProgram :: Program -> Bool
checkProgram pro = True

checkProgram2 pro =
  checkUndefinedRules
    && checkStartRule
    && checkDuplicateRules
    && checkPatternMatchFailure
  where
    checkUndefinedRules :: Bool
    checkUndefinedRules = fold foldUndef pro

    foldUndef :: TESTAlgebra Bool (String, [String]) [String] String Bool Bool Bool Bool
    -- foldUndef = undefined
    foldUndef = ((p1), (r1), (cs1, cs2), (c1, c2, c3, c4), (d1), (as1, as2), (a1), (pt1, pt2, pt3, pt4, pt5, pt6))
      where
        p1 xs = let (rules1, cmds1) = unzip xs in all (`elem` rules1) (concat cmds1)
        r1 s cmds = (s, cmds)
        cs1 = []
        cs2 cmd cmds = if null cmd then cmds else cmd : cmds
        c1 = ""
        c2 dir = ""
        c3 dir alts = ""
        c4 s = s
        d1 = False
        as1 = False
        as2 alt alts = False
        a1 pat cmds = False
        pt1 = False
        pt2 = False
        pt3 = False
        pt4 = False
        pt5 = False
        pt6 = False

    checkStartRule :: Bool
    -- checkStartRule = fold (\rules (name, _) _ _ _ _ _ _ -> (any (==True) rules, name == "start", (False,False), (False,False,False,False),False,(False,False),False,False) ) pro
    checkStartRule = fold foldAlgebra pro
    foldAlgebra :: TESTAlgebra Bool Bool Bool Bool Bool Bool Bool Bool
    foldAlgebra = ((p1), (r1), (cs1, cs2), (c1, c2, c3, c4), (d1), (as1, as2), (a1), (pt1, pt2, pt3, pt4, pt5, pt6))
      where
        p1 xs = any (== True) xs
        r1 s cmds = s == "start"
        cs1 = False
        cs2 cmd cmds = False
        c1 = False
        c2 dir = False
        c3 dir alts = False
        c4 s = False
        d1 = False
        as1 = False
        as2 alt alts = False
        a1 pat cmds = False
        pt1 = False
        pt2 = False
        pt3 = False
        pt4 = False
        pt5 = False
        pt6 = False

    checkDuplicateRules :: Bool
    checkDuplicateRules = fold foldDup pro

    foldDup :: TESTAlgebra Bool String Bool Bool Bool Bool Bool Bool
    foldDup = ((p1), (r1), (cs1, cs2), (c1, c2, c3, c4), (d1), (as1, as2), (a1), (pt1, pt2, pt3, pt4, pt5, pt6))
      where
        p1 xs = fst (foldr (\rule (res, seen) -> if rule `elem` seen then (False, rule : seen) else (res, rule : seen)) (True, []) xs)
        r1 s cmds = s
        cs1 = False
        cs2 cmd cmds = False
        c1 = False
        c2 dir = False
        c3 dir alts = False
        c4 s = False
        d1 = False
        as1 = False
        as2 alt alts = False
        a1 pat cmds = False
        pt1 = False
        pt2 = False
        pt3 = False
        pt4 = False
        pt5 = False
        pt6 = False

    checkPatternMatchFailure :: Bool
    checkPatternMatchFailure = fold foldPattern pro

    foldPattern :: TESTAlgebra Bool Bool Bool Bool Bool ([Pat], [Bool]) (Pat, Bool) Pat
    foldPattern = ((p1), (r1), (cs1, cs2), (c1, c2, c3, c4), (d1), (as1, as2), (a1), (pt1, pt2, pt3, pt4, pt5, pt6))
      where
        p1 xs = all (== True) xs
        r1 s cmds = cmds
        cs1 = True
        cs2 cmd cmds = cmd && cmds
        c1 = True
        c2 dir = True
        c3 dir alts = (all (`elem` fst alts) contents || PUnderscore `elem` fst alts) && all (== True) (snd alts)
        c4 s = True
        d1 = True
        as1 = ([], [])
        as2 alt alts = (fst alt : fst alts, snd alt : snd alts)
        a1 pat cmds = (pat, cmds)
        pt1 = PEmpty
        pt2 = PLambda
        pt3 = PDebris
        pt4 = PAsteroid
        pt5 = PBoundary
        pt6 = PUnderscore
        contents :: [Pat]
        contents = [PEmpty, PLambda, PDebris, PAsteroid, PBoundary]
