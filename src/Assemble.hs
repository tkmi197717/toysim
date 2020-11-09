module Assemble where

import Data.Array
import Data.List
import Toysim (Op(..), Arg(..))
import qualified Toysim as Toy
import qualified Machine as M
import Debug.Trace

assemble :: (Toy.Program, Toy.SymTable) -> M.Memory 
assemble (prog, table) = case foldl' phi (0, table) prog of
    (_, table') ->  case concatMap (psi table') prog of
        lls -> listArray (0, length lls - 1) lls

phi :: (Int, Toy.SymTable) -> (Int, (Op, Arg)) -> (Int, Toy.SymTable)
phi (i, tab) (_, (ope, _)) = if ope < LOAD then (succ i, tab)
                               else (i + 2, map inc tab)
    where 
        inc (k, v) = if i < v then (k, v + 1)
                     else (k, v)

psi :: Toy.SymTable -> (Int, (Op, Arg)) -> [M.Content]
psi _ (_, (NOP, Number n)) = [n]
psi tab (_, (ope, opd))
    = if ope < LOAD then [fromEnum ope]
      else [fromEnum ope, val] 
    where 
        val = case opd of 
            Number n -> 2 * n 
            Label lab -> case lookup lab tab of
                Just n -> 2 * n + 1