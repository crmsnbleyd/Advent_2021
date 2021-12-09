import qualified Data.Set as Set
import Data.List(transpose)

main = do
  inp <- readFile "4_1.txt"
  let ln =  filter (\x -> x /="\n" && x /="") . lines $ inp
  let (num: matList) = ln
  let matlist' = map (map (read :: String -> Int) . words) matList
  let numList = map (read :: String -> Int) (wordsWhen (==',') num)
  let matrixList = chunkList 5 matlist'
  print (payload numList matrixList)

payload :: [Int] -> [[[Int]]] -> Int
payload numbers matList = uncurry (*) (finalAndUnmarked numbers (map setify matList)) 

finalAndUnmarked :: [Int] -> [([Set.Set Int], [Set.Set Int])] -> (Int, Int)
finalAndUnmarked [] _ = error "we shouldnt get here"
finalAndUnmarked (x:xs) matSetList = if solved then (x, sum') else finalAndUnmarked xs mod
  where (solved, sum', mod) = checkBingo x matSetList []

checkBingo :: Int -> [([Set.Set Int], [Set.Set Int])] -> [([Set.Set Int], [Set.Set Int])] -> (Bool, Int, [([Set.Set Int], [Set.Set Int])] )
checkBingo n [] acc = (False, 0, acc)
checkBingo n [(rows, cols)] acc = if elem Set.empty minrow || elem Set.empty mincol then (True, summation, acc) else checkBingo n rest ((minrow,mincol):acc)
    where minrow = map (Set.delete n) rows
          mincol = map (Set.delete n) cols
          summation = sum (map sum minrow)
  
setify :: [[Int]] -> ([Set.Set Int], [Set.Set Int])
setify matrix = (map Set.fromList matrix, map Set.fromList (transpose matrix))

chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n lis = if length lis < n then [lis] else pfx : chunkList n sfx
  where (pfx, sfx) = splitAt n lis

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
