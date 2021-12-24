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

main :: IO()
main = do
  inp <- readFile "10.txt"
  -- print . (`match` []) . head . lines $ inp
  print . sum . map (`match` []) . lines $ inp

