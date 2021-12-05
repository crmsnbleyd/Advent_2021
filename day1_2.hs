import Data.List(foldl')
main :: IO ()
main = do
       lis <- readFile "1_1.txt"
       print . incrAmount . prepare . map (read :: String -> Int) . words $ lis

prepare :: [Int] -> [Int]
prepare = map Main.sum . zip3self 

zipself :: [b] -> [(b, b)]
zipself xs = xs `zip` tail xs

incrAmount :: (Ord a1, Num a2) => [a1] -> a2
incrAmount xs = foldl' (\r (x, y) -> if x < y then r+1 else r) 0 (zipself xs)
       
zip3self :: [c] -> [(c, c, c)]
zip3self xs = zip3 xs (tail xs) (drop 2 xs)

sum :: Num a => (a, a, a) -> a
sum (x,y,z) = x + y + z