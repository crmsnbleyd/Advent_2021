import qualified Data.Set as Set
import Data.Bifunctor(bimap)
import Data.List(transpose)

main = do
  inp <- readFile "4_1.txt"
  let ln =  filter (\x -> x /="\n" && x /="") . lines $ inp
  let (num: matList) = ln
  let matlist' = map (map (read :: String -> Int) . words) matList
  let numList = map (read :: String -> Int) (wordsWhen (==',') num)
  let matrixList = chunkList 5 matlist'
  -- print . bingos [3,18,50,82,90]. setify . head $ matrixList
  print (payload numList matrixList)

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

payload :: [Int] -> [[[Int]]] -> Int
payload numbers matList = uncurry (*) (finalMatrix numbers (map setify matList))

finalMatrix :: [Int] -> [([Set.Set Int], [Set.Set Int])] -> (Int, Int)
finalMatrix [] _ = error "bottom reached"
finalMatrix (n:ns) matList = case length reduced of
  0 -> bingos [n] (last matList)
  1 -> bingos (n:ns) (head reduced)
  _ -> finalMatrix ns reduced
  where reduced = markn n matList

markn :: Int -> [([Set.Set Int], [Set.Set Int])] -> [([Set.Set Int], [Set.Set Int])]
markn _ [] = []
markn n matList = filter (\(rows, cols) -> notElem Set.empty rows && notElem Set.empty cols) . map (bimap (map (Set.delete n)) (map (Set.delete n))) $ matList

bingos :: [Int] -> ([Set.Set Int], [Set.Set Int]) -> (Int, Int)
-- takes a list of ints and applies to single bingo table till we get bingo
-- and then returns the final int along with sum of all unmarked items
bingos [] _ = error "bottom of bingos reached"
bingos (n:ns) (rows, cols) = if Set.empty `elem` newrows || Set.empty `elem` newcols then (n, sum (map sum newrows)) else bingos ns (newrows, newcols)
    where (newrows, newcols) = (map (Set.delete n) rows, map (Set.delete n) cols)
