module Main where

import System.Environment
import Toysim 
import Text.Printf

main :: IO ()
main = do
    { f:_ <- getArgs 
    ; str <- readFile f
    ; drive (run (loadProg str))
    }

drive :: (Inputs -> [Output]) -> IO ()
drive r = interact (concat . map showOutput . r . lines )

showOutput :: Output -> String
showOutput o = case o of
    Left str -> printf "%s\n" str
    Right Nothing -> ""
    Right (Just i) -> printf "%d\n" i

read' :: String -> Int
read' s = case s of
    "" -> minBound 
    _ -> read s

