{-# LANGUAGE TupleSections #-}
import Data.List(intersect, delete)
import qualified Data.Set as S

main :: IO ()
main = do
  input <- readFile "5_1.txt"
  let inputList = lines input
  let pointsList = map stringToPoints inputList
  print . numberOfPoints . filterDiagonal $ pointsList
  
stringToPoints :: String -> ((Int, Int), (Int, Int))
stringToPoints ls = something pairs
  where pairs = filter (/= "->"). words $ ls
        something [x,y] = ( twoListToPair . map (read :: String -> Int). wordsWhen (==',') $ x, twoListToPair . map (read :: String -> Int). wordsWhen (==',') $ y)
        something _ = error "does not match specified inpuinterspersed x1 x2 x3 x4t format"
        twoListToPair [x,y] = (x,y)
        twoListToPair _ = error "cmon read the function name"

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
                                  
numberOfPoints :: [((Int, Int), (Int, Int))] -> Int
numberOfPoints lis = (length . S.fromList) (concatMap (\x -> concatMap (intersectionPointsOrthogonal x) (delete x lis)) lis)

intersectionPointsOrthogonal ((x1, y1),(x2, y2)) ((x3, y3), (x4, y4))
    | x1 == x2 && x3 == x4 && x3 == x1 = map (x1,) (pointList y1 y2 `intersect` pointList y3 y4)-- two vert lines
    | x1 == x2 && x3 == x4 = []
    | x1 == x2 = [(x1, y3) | ((x3 >= x1 && x4 <= x1) || (x3 <= x1 && x4 >= x1)) && ((y3 >= y1 && y3 <= y2) || (y3 <= y1 && y3 >= y2))]--vertical & horiz line (y3 == y4)
    | y1 == y2 && y3 == y4 && y3 == y1 = map (, y1) (pointList x1 x2 `intersect` pointList x3 x4)-- two horiz lines
    | y1 == y2 && y3 == y4 = []
    | y1 == y2 = [(x3, y1) | ((x3 >= x1 && x3 <= x2) || (x3 <= x1 && x3 >= x2)) && ((y1 >= y3 && y1 <= y4) || (y1 <= y3 && y3 >= y4))]--horiz and vert line (x3 == x4)
    | otherwise = error "only orthogonal points"

pointList a b = if a < b then [a..b] else [b..a]

filterDiagonal :: [((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))]
filterDiagonal = filter (\((x1,y1),(x2,y2)) -> x1 == x2 || y1 == y2)