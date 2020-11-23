module Main0 where

import System.Environment
import Toysim 

main :: IO ()
main = undefined -- do
    -- { f:_ <- getArgs 
    -- ; str <- readFile f
    -- ; drive (run (loadProg str))
    -- }

drive :: (Inputs -> [Int]) -> IO ()
drive r = interact (unlines . map show . r . map read . lines )
