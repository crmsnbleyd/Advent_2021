main :: IO ()
main = print "to be implemented!"

stringToPoints :: String -> ([Int], [Int])
stringToPoints ls = something pairs
  where pairs = filter (/= "->"). words $ ls
        something [x,y] = (map (read :: String -> Int). wordsWhen (==',') $ x, map (read :: String -> Int). wordsWhen (==',') $ y) 
        something _ = error "does not match specified input format"

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
