import Data.List(foldl')
main = do
  inp <- readFile "8.txt"
  let inpList = map words . lines $ inp
  let cutList = map (drop 11) inpList
  let num = sum (map (foldl' (\acc x -> if length x `elem` [2,3,4,7] then acc+1 else acc) 0) cutList)
  print num
