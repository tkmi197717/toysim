module Main1 where

import System.Environment
import Toysim 
import Assemble
import Machine as M

main :: IO ()
main = do
    { f:_ <- getArgs 
    ; str <- readFile f
    ; let mem = assemble (loadProg str)
    ; print mem 
    ; interact (M.runProg mem)
    }

-- drive :: (Inputs -> [Int]) -> IO ()
-- drive r = interact (unlines . map show . r . map read . lines )
