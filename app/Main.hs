module Main where

import System.Environment
import Toysim 
import Assemble

main :: IO ()
main = do
    { f:_ <- getArgs 
    ; str <- readFile f
    ; print (assemble (loadProg str))
    }

drive :: (Inputs -> [Int]) -> IO ()
drive r = interact (unlines . map show . r . map read . lines )
