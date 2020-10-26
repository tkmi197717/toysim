module Machine where

import Toysim (Op(..))
import Data.Array
import Debug.Trace

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
    STOP -> trace "stop" (mem, ac, -1, ins, Nothing)
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

getval :: Memory -> Content -> Int
getval mem c = if even c 
    then c `div` 2
    else mem ! (c `div` 2)

update :: Memory -> Addr -> Int -> Memory
update mem a i = mem // [(a, i)]

sample :: Memory 
sample = listArray (0, 13) [1, 10, 19, 6, 13, 5, 13, 8, 1, 4, 13, 2, 3, 0] 

initstate :: MState 
initstate = (sample, 0, 0, [1,2,3,0], Nothing)