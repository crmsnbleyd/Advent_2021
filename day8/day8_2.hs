import Data.List (sortOn)
import qualified Data.Set as S

decode :: [String] -> (String->Int)
decode lis = func
   where [one, seven, four, _ , _, _,
          six_one, six_two, six_three, eight] = map S.fromList . sortOn length $ lis
         f_g = S.difference four one
         e_g_b = foldr1 S.union . map (S.difference eight) $ [six_one, six_two, six_three]
         e_g = S.difference e_g_b one
         b = S.difference e_g_b e_g
         c = S.difference one b
         e = S.difference e_g f_g
         g = S.difference e_g e
         f = S.difference f_g g
         six = S.difference eight b
         nine = S.difference eight e
         zero = S.difference eight g
         five = S.difference six e
         two = S.difference (S.difference eight f) c
         three = S.difference nine f
         func str | strSet == one = 1
                  | strSet == two = 2
                  | strSet == three = 3
                  | strSet == four = 4
                  | strSet == five = 5
                  | strSet == six = 6
                  | strSet == seven = 7
                  | strSet == eight = 8
                  | strSet == nine = 9
                  | strSet == zero = 0
                  | otherwise = error "seven segment display"
                  where strSet = S.fromList str

prepareInput str = (first, tail second)
    where worded = words str
          (first, second) = splitAt 10 worded

valuesToInt [w,x,y,z] = w*1000 + x*100 + y*10 + z
valuesToInt _ = error "four int list only"

main = do
  inp <- readFile "8.txt"
  let res = sum . map ((\(l,r)-> valuesToInt . map (decode l) $ r). prepareInput) . lines $ inp
  print res
