import System.Environment
import Data.List
import qualified Mol2Parser

main = do
		patternStrs <- readFile "patterns"
		subsFileNames <- readFile "subs"
		subs <- mapM readFile $ lines subsFileNames
		mapM putStrLn $ map show $ map (calcDescrValues (zip (lines subsFileNames) subs)) (map words (lines patternStrs))

calcDescrValues subs pattern = (pattern, map (\x -> (fst x, getMaxCycleLength (map read pattern::[Int]) (lines (snd x)))) subs)

getMaxCycleLength pattern content = (`div` patternLen) $ maximum $ (startTraverse pattern patternLen) $ Mol2Parser.getBonds content
	where patternLen = length pattern

startTraverse pattern patternLen graph =
	foldl 	(\acc ((a, b), w) -> if (head pattern) == w
				then
					(max (traverse [((a, b), w)] b pattern patternLen 1 1 graph) (traverse [((a, b), w)] a pattern patternLen 1 1 graph)) : acc
				else
					acc
			)
			[]
			graph

traverse visited currVert pattern patternLen patternIndex currDepth graph =
	foldl 	(\acc ((a, b), w) -> if (w == (pattern !! patternIndex)) && (a == currVert || b == currVert) && (notElem ((a, b), w) visited)
				then
					max acc (traverse (((a, b), w):visited) (if a == currVert then b else a)  pattern patternLen (mod (patternIndex + 1) patternLen) (currDepth + 1) graph)
				else
					acc
			)
			currDepth
			graph

