{-# LANGUAGE TupleSections #-}
import Data.List(intersect)
main :: IO ()
main = print "to be implemented!"

stringToPoints :: String -> ((Int, Int), (Int, Int))
stringToPoints ls = something pairs
  where pairs = filter (/= "->"). words $ ls
        something [x,y] = ( twoListToPair . map (read :: String -> Int). wordsWhen (==',') $ x, twoListToPair . map (read :: String -> Int). wordsWhen (==',') $ y)
        something _ = error "does not match specified input format"
        twoListToPair [x,y] = (x,y)
        twoListToPair _ = error "cmon read the function name"

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

intersectionPointsOrthogonal :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
intersectionPointsOrthogonal ((x1, y1),(x2, y2)) ((x3, y3), (x4, y4))
    | x1 == x2 && x3 == x4 && x3 == x1 = map (x1,) (pointList y1 y2 `intersect` pointList y3 y4)-- two vert lines
    | x1 == x2 && x3 == x4 = []
    | x1 == x2 = [(x1, y3) | interspersed y1 y2 y3 y4]--vertical & horiz line (y3 == y4)
    | y1 == y2 && y3 == y4 && y3 == y1 = map (, y1) (pointList x1 x2 `intersect` pointList x3 x4)-- two horiz lines
    | y1 == y2 && y3 == y4 = []
    | y1 == y2 = [(x3, y1) | interspersed y1 y2 y3 y4]--horiz and vert line (x3 == x4)
    | otherwise = []

pointList a b = if a < b then [a..b] else [b..a]

interspersed :: Int -> Int -> Int -> Int -> Bool -- takes a b c d and sees if any overlap b/w a-b line and c-d lines
interspersed a b c d
  | a == b = c == d
  | a < b && c <= d = (c <=b && c >= a) || (a <= d && a >= c)
  | b < a && c <= d = (c >=b && c <= a) || (b <= d && b >= c)
  | a < b && c >= d = (d <=b && d >= a) || (a <= c && a >= d)
  | b < a && c >= d = (d >=b && d <= a) || (b <= c && b >= d)
  | otherwise = False
