module Mol2Parser
( getBlock
, getFormula
, getBonds
, getAtoms
) where

import Data.List
import Debug.Trace

tiposPrefix = "@<TRIPOS>"
data ReadState = StartState | InState | EndState

getFormula xs = map (\x -> (head x, length x)) $ group . sort $ map (last . take 6 . words) $ getAtoms xs
getAtoms xs = getBlock "ATOM" xs
getBonds xs = foldl (\acc row -> ((\(c1:c2:c3:c4:rest) -> ((read c2::Int, read c3::Int), read c4::Int))(words row)) : acc) [] $ getBlock "BOND" xs

-------------------------------
--- Returns block rows
--- name - block name
--- content - list of file rows
-------------------------------
getBlock name content = snd $ foldl (collect name) (StartState, []) content

collect name res [] = res
collect name (StartState, result) x
	| isPrefixOf x (tiposPrefix ++ name) = (InState, result)
	| otherwise = (StartState, result)
collect name (InState, result) x
	| isPrefixOf tiposPrefix x = (EndState, result)
	| otherwise = (InState, x:result)
collect name (EndState, result) x = (EndState, result)
