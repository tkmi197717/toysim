

module Toysim where
    
import Data.Array 
import Data.Char ( isDigit, isSpace, toUpper )
import Data.List
import Data.Maybe
import Debug.Trace
import Text.Printf
--データ型
data Op
    = NOP
    | GET 
    | PRINT
    | STOP
    | LOAD
    | STORE
    | ADD
    | SUB
    | GOTO
    | IFPOS
    | IFZERO
    deriving (Eq,Ord,Enum,Bounded,Show,Read)

data Arg
    = None
    | Number Int
    | Label String
    deriving (Eq,Show)

readArg :: String -> Arg
readArg a = if all isDigit a
    then Number (read a)
    else Label a
--シンボルテーブル
type Instrustion = (Op, Arg)

type Program = [(Int,Instrustion)]

type SymTable = [(String, Int)]

type DebugInfo = [BreakPoint] 

type BreakPoint = Int
--読み込み
loadProg :: String -> (Program, SymTable)
loadProg s = case mapAccumL psi (0, []) (lines s) of
    ((n, tab), prog) -> (prog, tab)

psi :: (Int, SymTable) -> String -> ((Int, SymTable), (Int, Instrustion))
psi (i,tab) (c:cs) = if isSpace c 
    then case words cs of
        [o] -> ((succ i, tab), (i, (read (upCase o), None)))
        [o, a] -> ((succ i, tab), (i, (read (upCase o), readArg (upCase a))))
    else case words (c:cs) of
        [lab, o] -> if all isDigit o
            then ((succ i, (upCase lab, i):tab ), (i, (NOP, Number (read o))))
            else ((succ i, (upCase lab, i):tab), (i, (read (upCase o), None)))
        [lab, o, a]  -> ((succ i, (upCase lab, i):tab), (i, (read (upCase o), readArg (upCase a))))

upCase :: String -> String
upCase = map toUpper

sample :: String
sample = unlines
    ["TOP get"
    ,"    IFZERO bot"
    ,"    ADD SUM"
    ,"    STORE SUM"
    ,"    GOTO top"
    ,"BOT LOAD SUM"
    ,"    PRINT"
    ,"    STOP"
    ,"SUM 0"
    ]

sampleInput :: [Int]
sampleInput = [3,1,4,1,5,9,0]

type ToyState = (Program, SymTable, Acc, Pc, Inputs, Output)
type Acc = Int
type Pc = Int
type Inputs = [String]
-- type Inputs = [Int]
type Output = Either String (Maybe Int)

-- program_ :: ToyState -> Program 
-- program_ (p, _, _, _, _, _) = p

-- symTable_ :: ToyState -> SymTable
-- symTable_ (_, s, _, _, _, _) = s

-- acc_ :: ToyState -> Acc
-- acc_ (_, _, a, _, _, _) = a

-- pc_ :: ToyState -> Pc
-- pc_ (_, _, _, p, _, _) = p

-- inputs_ :: ToyState -> Inputs
-- inputs_ (_, _, _, _, i, _) = i

--ステップ実行
output :: ToyState -> Output
output (_, _, _, _, _, o) = o

run :: (Program, SymTable) -> Inputs -> [Output]
run (prog, tab) ins = trace "set break points: " $
    case ins of
        i:is -> map output (exec' bs (prog, tab, 0, 0, is, Right Nothing))
                where 
                    bs = case map toUpper i of
                        i' | "A" `isPrefixOf` i' -> [0 .. length prog]
                           | otherwise           -> readInts i

readInts :: String -> [Int]
readInts = read

-- exec :: ToyState -> [ToyState]
-- exec st = st : rests
--     where 
--         rests = if isFinal st then [] 
--                 else st' : exec (step st')
--         st' = case st of
--             (prog, tab, acc, pc, i:is, _ ) -> (prog, tab, acc, pc, is, disp st)

exec' :: DebugInfo -> ToyState -> [ToyState]
exec' bs st = st : rests
    where 
        rests = if isFinal st then [] 
                else extend st
        extend st@(prog, tab, acc, pc, iis, _) 
            = if elem pc bs 
                then (prog, tab, acc, pc, iis, disp st) : case iis of 
                    i:is -> if "C" `isPrefixOf` map toUpper i 
                        then exec' [] (step (prog, tab, acc, pc, is, Right Nothing)) 
                        else exec' bs (step (prog, tab, acc, pc, is, Right Nothing))
                else exec' bs (step st)


disp :: ToyState -> Either String (Maybe Int)
disp (prog, tab, acc, pc, _, _ ) = Left $ dispProg tab pc prog ++ dispAcc acc

dispAcc :: Acc -> String 
dispAcc acc = printf "acc: %d\n" acc

dispProg :: SymTable -> Pc -> Program -> String 
dispProg tab pc prog = unlines $ map (showLine tab pc) prog 

showLine :: SymTable -> Pc -> (Int, Instrustion) -> String
showLine tab pc (i, inst) = if pc == i 
    then printf "> %06d: %s" i (showInst tab inst)
    else printf "  %06d: %s" i (showInst tab inst)

showInst :: SymTable -> Instrustion -> String
showInst tab inst = case inst of
    (NOP, Number n) -> show n  
    (op, arg) -> show op ++ case arg of
        None -> ""
        Number n -> " " ++ show n
        Label l -> " " ++ l ++ l'
                    where 
                        l' = "(" ++ show (lookingup arg tab) ++ ")"  

isFinal :: ToyState -> Bool
isFinal (_, _, _, -1, _, _) = True
isFinal _ = False

step :: ToyState -> ToyState
step st = execute (decode (fetch st)) st
-- step (prog, tab, acc, pc, ins, _)
--     = case genericIndex prog pc of
--         (_, (op, arg)) -> case op of 
--             GET -> trace "Input number" $ case ins of { i : is -> (prog, tab, i, succ pc, is, Nothing) }
--             PRINT -> (prog, tab, acc, succ pc, ins, Just acc)
--             STOP -> trace "stop" (prog, tab, acc, -1, ins, Nothing)
--             LOAD -> (prog, tab, getval prog tab arg, succ pc, ins, Nothing)
--             STORE -> (update prog tab acc arg, tab, acc, succ pc, ins, Nothing)
--             ADD -> (prog, tab, acc + getval prog tab arg, succ pc, ins, Nothing)
--             SUB -> (prog, tab, acc - getval prog tab arg, succ pc, ins, Nothing)
--             GOTO -> (prog, tab, acc, lookingup arg tab, ins, Nothing)
--             IFZERO -> (prog, tab, acc, if acc == 0 then lookingup arg tab else succ pc, ins, Nothing)
--             IFPOS -> (prog, tab, acc, if acc >= 0 then lookingup arg tab else succ pc, ins, Nothing)

fetch :: ToyState -> Instrustion 
fetch (prog, _, _, pc, _, _) = case genericIndex prog pc of
    (_, (op, arg)) -> (op, arg)

decode :: Instrustion -> (ToyState -> ToyState)
decode (op, arg) (prog, tab, acc, pc, ins, _) = case op of
    GET -> trace "Input number" $ case ins of { i : is -> (prog, tab, read i, succ pc, is, Right Nothing) }
    PRINT -> (prog, tab, acc, succ pc, ins, Right (Just acc))
    STOP -> trace "stop" (prog, tab, acc, -1, ins, Right Nothing)
    LOAD -> (prog, tab, getval prog tab arg, succ pc, ins, Right Nothing)
    STORE -> (update prog tab acc arg, tab, acc, succ pc, ins, Right Nothing)
    ADD -> (prog, tab, acc + getval prog tab arg, succ pc, ins, Right Nothing)
    SUB -> (prog, tab, acc - getval prog tab arg, succ pc, ins, Right Nothing)
    GOTO -> (prog, tab, acc, lookingup arg tab, ins, Right Nothing)
    IFZERO -> (prog, tab, acc, if acc == 0 then lookingup arg tab else succ pc, ins, Right Nothing)
    IFPOS -> (prog, tab, acc, if acc >= 0 then lookingup arg tab else succ pc, ins, Right Nothing)

execute :: (ToyState -> ToyState) -> (ToyState -> ToyState)
execute = id 

getval :: Program -> SymTable -> Arg -> Int
getval prog tab arg = case arg of 
    Number n -> n
    Label lab -> case lookup lab tab of 
        Nothing -> error ("unknown " ++ lab)
        Just m -> case lookup m prog of 
            Nothing -> error ("out of program range" ++ show m ++ ": " ++ show prog)
            Just (NOP, Number k) -> k

lookingup :: Arg -> SymTable -> Int
lookingup a tab = case a of 
    Label lab -> case lookup lab tab of 
        Nothing -> error ("unknown " ++ lab)
        Just m -> m

update :: Program -> SymTable -> Acc -> Arg -> Program
update prog tab acc arg = case lookingup arg tab of
    i -> case genericSplitAt i prog of 
        (ps, _:qs) -> ps ++ (i, (NOP, Number acc)) : qs