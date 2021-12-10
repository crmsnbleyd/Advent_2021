{-# LANGUAGE TupleSections #-}
import Data.List(intersect, delete)
import qualified Data.Set as S

main :: IO ()
main = do
  input <- readFile "5_1.txt"
  let inputList = lines input
  let pointsList = map stringToPoints inputList
  print . numberOfPoints $ pointsList
  
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

intersectionPointsOrthogonal a@((x1, y1),(x2, y2)) b@((x3, y3), (x4, y4))
    | x1 == x2 && x3 == x4 && x3 == x1 = map (x1,) (pointList y1 y2 `intersect` pointList y3 y4)-- two vert lines
    | x1 == x2 && x3 == x4 = []
    | x1 == x2 = [(x1, y3) | ((x3 >= x1 && x4 <= x1) || (x3 <= x1 && x4 >= x1)) && ((y3 >= y1 && y3 <= y2) || (y3 <= y1 && y3 >= y2))]--vertical & horiz line (y3 == y4)
    | y1 == y2 && y3 == y4 && y3 == y1 = map (, y1) (pointList x1 x2 `intersect` pointList x3 x4)-- two horiz lines
    | y1 == y2 && y3 == y4 = []
    | y1 == y2 = [(x3, y1) | ((x3 >= x1 && x3 <= x2) || (x3 <= x1 && x3 >= x2)) && ((y1 >= y3 && y1 <= y4) || (y1 <= y3 && y3 >= y4))]--horiz and vert line (x3 == x4)
    | otherwise = intersectDiagonal a b

pointList a b = if a < b then [a..b] else [b..a]

-- 1,2 to 4,5 line will have 1,2 2,3 3,4 4,5 i.e we just need direction vector to get the points

intersectDiagonal :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
intersectDiagonal linea@((x1,y1), (x2,y2)) lineb@((x3,y3),(x4,y4))
    | x1 == x2 = [(x1,yFirstCase) | not ((yFirstCase > y1 && yFirstCase > y2) || (yFirstCase < y1 && yFirstCase < y2))]
    | y1 == y2 = [(xSecondCase ,y1) | not ((xSecondCase > x1 && xSecondCase > x2) || (xSecondCase< x1 && xSecondCase< x2))]
    | x3 == x4 = [(x3,yThirdCase) | not ((yThirdCase > y3 && yThirdCase > y4) || (yThirdCase < y3 && yThirdCase < y4))]
    | y3 == y4 = [(xFourthCase ,y3) | not ((xFourthCase > x3 && xFourthCase > x4) || (xFourthCase< x3 && xFourthCase< x4))]
    | mFirstCase == mThirdCase && cFirstCase /= cThirdCase = []
    | mFirstCase == mThirdCase && ((x1 > x3 && x1 > x4 && x2 > x3 && x2 > x4) || (x1 < x3 && x1 < x4 && x2 < x3 && x2 < x4)) = []
    | mFirstCase == mThirdCase = [(x, mFirstCase*x + cFirstCase)|x <- pointList x3 x4 `intersect` pointList x1 x2]
    | otherwise = [(xFinalCase, yFinalCase)| 
    (((xFinalCase <= x2 && xFinalCase >= x1) || (xFinalCase >= x2 && xFinalCase <= x1))
    &&((xFinalCase <= x3 && xFinalCase >= x4) || (xFinalCase >= x3 && xFinalCase <= x4)))
    &&(((yFinalCase <= y2 && yFinalCase >= y1) || (yFinalCase >= y2 && yFinalCase <= y1))
    &&((yFinalCase <= y3 && yFinalCase >= y4) || (yFinalCase >= y3 && yFinalCase <= y4)))]
    where yFirstCase = x1 * mFirstCase + cFirstCase
          xSecondCase = round (fromIntegral (y1 - cFirstCase)/fromIntegral mFirstCase)
          (mFirstCase, cFirstCase) = directionVectorC lineb
          yThirdCase = x3 * mThirdCase + cThirdCase
          xFourthCase = round (fromIntegral (y1 - cThirdCase)/fromIntegral mThirdCase)
          (mThirdCase, cThirdCase) = directionVectorC linea
          xFinalCase = round (fromIntegral (cThirdCase-cFirstCase)/fromIntegral (mFirstCase - mThirdCase))
          yFinalCase = mFirstCase * xFinalCase + cFirstCase

directionVectorC:: ((Int, Int), (Int, Int)) -> (Int, Int) -- infinity not allowed
directionVectorC ((x1,y1), (x2,y2)) = (m,c)
    where m = round (fromIntegral (y2-y1)/fromIntegral (x2-x1))
          c = y1 - m*x1