import Data.List(foldl')
updateFreq (zeroFreq, oneFreq) ch 
    | ch == '1' = (zeroFreq, oneFreq+1)
    | ch == '0' = (zeroFreq+1, oneFreq)
    | otherwise = (zeroFreq, oneFreq)

newFreqs freqs binNumber = map (\(x, y) -> updateFreq x y) (zip freqs binNumber)

getDigitTuple (zeroFreq, oneFreq) = if zeroFreq > oneFreq
     then ('0', '1')
     else ('1', '0')

getGammaEpsilon :: [(Int, Int)] -> ([Char], [Char])
getGammaEpsilon = unzip . map (getDigitTuple)

binaryToDec :: String -> Int
binaryToDec num = foldr (\(val, pos) acc -> if val == '1' then acc + truncate (2 ^^ pos) else acc) 0 (zip num poss)
    where poss = reverse [0..(length num)-1]

payload b a = getGammaEpsilon (foldl' (newFreqs) b a)

payload' (gamma, epsilon) = (binaryToDec gamma) * (binaryToDec epsilon)

main = do
    inp <- readFile "3_1.txt"
    let myInp = lines inp
    let accs = repeat (0,0)
    let printer = payload' $ payload accs myInp
    print printer