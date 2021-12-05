import Data.List(foldl')
zipself :: [b] -> [(b, b)]
zipself xs = xs `zip` tail xs
incrAmount :: (Ord a1, Num a2) => [a1] -> a2
incrAmount xs = foldl' (\r (x, y) -> if x < y then r+1 else r) 0 (zipself xs)
main :: IO ()
main = do
       lis <- readFile "1_1.txt"
       print . incrAmount . map (read :: String -> Int) . words $ lis