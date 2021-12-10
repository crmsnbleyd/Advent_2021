import Data.List(group, sort, foldl')

main = print . breakingRepeat 256 $ input
lantern 0 = [6,8]
lantern n = [n-1]

repeatConcatMapLantern :: Int -> [Int] -> [Int]
repeatConcatMapLantern 1 lis = concatMap lantern lis
repeatConcatMapLantern n lis = repeatConcatMapLantern (n-1) . concatMap lantern $ lis

breakingRepeat n lis = foldl' (\acc (_, len) -> len+acc) 0 . concatMap (\(item, len) -> runfish len item n) $ numList
    where numList = map (\x -> (head x, length x)). group . sort $ lis


runfish runlength digit len
  | len <= 40 =  map (\x ->(head x, length x * runlength)). group . sort  $ repeatConcatMapLantern len [digit]
  | otherwise = concatMap (\(d, ln) -> runfish ln d (len-40))(runfish runlength digit 40)

input :: [Int]
input = [1,1,1,1,1,5,1,1,1,5,1,1,3,1,5,1,4,1,5,1,2,5,1,1,1,1,3,1,4,5,1,1,2,1,1,1,2,4,3,2,1,1,2,1,5,4,4,1,4,1,1,1,4,1,3,1,1,1,2,1,1,1,1,1,1,1,5,4,4,2,4,5,2,1,5,3,1,3,3,1,1,5,4,1,1,3,5,1,1,1,4,4,2,4,1,1,4,1,1,2,1,1,1,2,1,5,2,5,1,1,1,4,1,2,1,1,1,2,2,1,3,1,4,4,1,1,3,1,4,1,1,1,2,5,5,1,4,1,4,4,1,4,1,2,4,1,1,4,1,3,4,4,1,1,5,3,1,1,5,1,3,4,2,1,3,1,3,1,1,1,1,1,1,1,1,1,4,5,1,1,1,1,3,1,1,5,1,1,4,1,1,3,1,1,5,2,1,4,4,1,4,1,2,1,1,1,1,2,1,4,1,1,2,5,1,4,4,1,1,1,4,1,1,1,5,3,1,4,1,4,1,1,3,5,3,5,5,5,1,5,1,1,1,1,1,1,1,1,2,3,3,3,3,4,2,1,1,4,5,3,1,1,5,5,1,1,2,1,4,1,3,5,1,1,1,5,2,2,1,4,2,1,1,4,1,3,1,1,1,3,1,5,1,5,1,1,4,1,2,1]
