import Data.List(sort, foldl')
match :: [Char] -> [Char] -> Int
match (x:xs) stack
  | x `elem` "([{<" = match xs (x:stack)
  | x == ')' = if stack /= [] && s == '(' then match xs t else 3
  | x == ']' = if stack /= [] && s == '[' then match xs t else 57
  | x == '}' = if stack /= [] && s == '{' then match xs t else 1197
  | x == '>' = if stack /= [] && s == '<' then match xs t else 25137
  | otherwise = error "illegal character"
  where (s:t) = stack
match [] _ = 0

complete :: [Char] -> [Char] -> Int
complete (x:xs) stack
  | x `elem` "([{<" = complete xs (x:stack)
  | x == ')' && stack /= [] && s == '(' = complete xs t
  | x == ']' && stack /= [] && s == '[' = complete xs t
  | x == '}' && stack /= [] && s == '{' = complete xs t
  | x == '>' && stack /= [] && s == '<' = complete xs t
  | otherwise = error "illegal character"
  where (s:t) = stack

complete [] stack = foldl' (\acc ch -> acc*5 + func ch) 0 stack
  where func '(' = 1
        func '[' = 2
        func '{' = 3
        func '<' = 4
        func _ = 0

main :: IO()
main = do
  inp <- readFile "10.txt"
  print . foldl' (\acc str -> acc + match str []) 0 . lines $ inp
  let completeLines =  filter (\str -> match str [] == 0) . lines $ inp
  print . (!! (length completeLines `div` 2)) . sort . map (`complete` []) $ completeLines