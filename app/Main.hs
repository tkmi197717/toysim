module Main where

import System.Environment
import Toysim 

main :: IO ()
main = do
    { f:_ <- getArgs 
    ; str <- readFile f
    ; drive (run (loadProg str))
    }

drive :: (Inputs -> [Output]) -> IO ()
drive r = interact (unlines . map show . r . map read' . lines )

read' :: String -> Int
read' s = case s of
    "" -> minBound 
    _ -> read s

