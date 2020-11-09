module Machine where

import Toysim (Op(..))
import Data.Array
import Debug.Trace
import Data.Maybe


type Memory = Array Addr Content
type Content = Int
type Addr = Int
type MState = (Memory, Acc, Pc, Inputs, Output)
type Acc = Int
type Pc = Int
type Inputs = [Int]
type Output = Maybe Int

step :: MState -> MState
step st = execute (decode (fetch st)) st

fetch :: MState -> Content
fetch (mem, _, pc, _, _) = mem ! pc 

decode :: Content -> (MState -> MState)
decode c (mem, ac, pc, ins, _) = case toEnum c :: Op of 
    GET -> trace "Input number" $ case ins of { i : is -> (mem, i, succ pc, is, Nothing) }
    PRINT -> (mem, ac, succ pc, ins, Just ac)
    STOP -> trace "stopped" (mem, ac, -1, ins, Nothing)
    LOAD -> (mem, getval mem (succ pc), succ (succ pc), ins, Nothing)
    STORE -> (update mem (getaddr (mem ! succ pc)) ac, ac, succ (succ pc), ins, Nothing)
    ADD -> (mem, ac + getval mem (succ pc), succ (succ pc), ins, Nothing)
    SUB -> (mem, ac - getval mem (succ pc), succ (succ pc), ins, Nothing)
    GOTO -> (mem, ac, getaddr (mem ! succ pc), ins, Nothing)
    IFZERO -> (mem, ac, if ac == 0 then getaddr (mem ! succ pc) else succ (succ pc), ins, Nothing)
    IFPOS -> (mem, ac, if ac >= 0 then getaddr (mem ! succ pc) else succ (succ pc), ins, Nothing)

execute :: (MState -> MState) -> (MState -> MState)
execute = id

getaddr :: Content -> Addr
getaddr c = c `div` 2

getval :: Memory -> Addr -> Int
getval mem a = if even c 
    then c `div` 2
    else mem ! (c `div` 2)
    where 
        c = mem ! a 

update :: Memory -> Addr -> Int -> Memory
update mem a i = mem // [(a, i)]

sample :: Memory 
sample = listArray (0, 13) [1, 10, 19, 6, 27, 5, 27, 8, 1, 4, 27, 2, 3, 0] 

initstate :: Memory -> Inputs -> MState 
initstate mem ins = (mem, 0, 0, ins, Nothing)

execProg :: MState -> [MState]
execProg st = st : rests 
    where 
        rests = if isFinal st then []
                else execProg (step st)

isFinal :: MState -> Bool
isFinal (_, _, -1, _, _) = True
isFinal _ = False

runProg :: Memory -> String -> String
runProg mem = unlines . map show . mapMaybe output . tracing debug . execProg . initstate mem . map read . lines

output :: MState -> Output
output (_, _, _, _, o) = o 

tracing :: Bool -> [MState] -> [MState]
tracing flag ss = if flag then trace (unlines (map showState ss)) ss else ss

-- 機能しなかったので生やした。確認。5。拡張。

debug :: Bool 
debug = False 

showState :: MState -> String
showState (mem, ac, pc, _, _) = show mem ++ " acc: " ++ show ac ++ " pc: " ++ show pc