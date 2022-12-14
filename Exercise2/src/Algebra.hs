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

 
type TESTAlgebra pro rule cmds cmd dir alts alt pat = (
    ([rule] -> pro),                               -- Program
    (String -> cmds -> rule), 
    (cmds, cmd -> cmds -> cmds), 
    (cmd, dir -> cmd, dir -> alts -> cmd, String -> cmd), 
    (dir), 
    (alts, alt -> alts -> alts), 
    (pat -> cmds -> alt), 
    (pat) )

fold :: TESTAlgebra p r cs c d as a pt -> Program -> p
fold ((p1),(r1),(cs1,cs2),(c1,c2,c3,c4),(d1),(as1,as2),(a1),(pt)) = fold'
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
        foldPt (PEmpty) = pt 
        foldPt (PLambda) = pt 
        foldPt (PDebris) = pt 
        foldPt (PAsteroid) = pt 
        foldPt (PBoundary) = pt 
        foldPt (PUnderscore) = pt




-- Exercise 6

checkProgram :: Program -> Bool
checkProgram pro = True

checkProgram2 pro = checkUndefinedRules && checkStartRule && 
                   checkDuplicateRules && checkPatternMatchFailure
    where
        checkUndefinedRules = False

        checkStartRule :: Bool
        -- checkStartRule = fold (\rules (name, _) _ _ _ _ _ _ -> (any (==True) rules, name == "start", (False,False), (False,False,False,False),False,(False,False),False,False) ) pro
        checkStartRule = fold foldAlgebra pro
        foldAlgebra :: TESTAlgebra Bool Bool Bool Bool Bool Bool Bool Bool
        foldAlgebra = ((p1),(r1),(cs1,cs2),(c1,c2,c3,c4),(d1),(as1,as2),(a1),(pt))
            where
                p1 xs = any (==True) xs
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
                pt = False
        

        checkDuplicateRules :: Bool
        checkDuplicateRules = fold foldDup pro
       
        foldDup :: TESTAlgebra Bool String Bool Bool Bool Bool Bool Bool
        foldDup = ((p1),(r1),(cs1,cs2),(c1,c2,c3,c4),(d1),(as1,as2),(a1),(pt))
            where
                p1 xs = fst (foldr (\rule (res,seen) -> if rule `elem` seen then (True,rule:seen) else (res,rule:seen))  (False,[]) xs )
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
                pt = False

        checkPatternMatchFailure = False