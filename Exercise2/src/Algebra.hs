module Algebra where

import Model


-- Exercise 5

-- lecture notes page 86

type PAlgebra p = (p -> p)               -- Program
                -- String -> r -> r,       -- rule
                -- r, r -> r -> r,         -- cmds
                -- r,r -> r, r -> r -> r,  -- cmd
                -- r,                      --dir
                -- r, r -> r -> r,         -- alts
                -- r -> r -> r,            -- alt
                -- r)                      -- pat

-- fold :: Algebra r ->
fold = undefined



-- Exercise 6

checkProgram :: Program -> Bool
checkProgram = undefined