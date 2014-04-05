import System.Environment
import Data.List
import qualified Mol2Parser

main = do
  patternStrs <- readFile "patterns"
  subsFileNames <- readFile "subs"
  subs <- mapM readFile $ lines subsFileNames
  mapM putStrLn
    (map show
      (map (calcDescrValues (zip (lines subsFileNames) subs))
      (map words (lines patternStrs))))

calcDescrValues subs pattern =
  (pattern, map
    (\x -> (fst x, getMaxCycleLength patternInInt (lines (snd x))))
    subs)
  where patternInInt = map read pattern

getMaxCycleLength :: [Int] -> [[Char]] -> Int
getMaxCycleLength pattern content = (`div` patternLen)
  $ maximum $ (startTraverse pattern patternLen) $ Mol2Parser.getBonds content
  where patternLen = length pattern

startTraverse pattern patternLen graph =
  foldl (\acc ((a, b), w) ->
    (max
      (traverse [] b pattern patternLen 0 0 graph)
      (traverse [] a pattern patternLen 0 0 graph))
    : acc
  )
  []
  graph

traverse visited currVert pattern patternLen patternIndex currDepth graph =
  foldl (\acc ((a, b), w) ->
  	if (w == (pattern !! patternIndex)) && (a == currVert || b == currVert) &&
  	  (notElem ((a, b), w) visited)
    then
      let nextVert = if a == currVert then b else a in
        max acc
          (traverse (((a, b), w):visited) nextVert
            pattern patternLen nextPatternIndex nextDepth graph)
    else
      acc
  )
  currDepth
  graph
  where
    nextPatternIndex = mod (patternIndex + 1) patternLen
    nextDepth = currDepth + 1

