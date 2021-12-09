input = []

diff op m = map (\ e -> op (abs $ e - m))

solve op l = minimum [sum $ diff op c l | c <- [minimum l .. maximum l]]

main = do
  print $ solve id input
  print $ solve (\x -> (x + 1) * x `div` 2) input
