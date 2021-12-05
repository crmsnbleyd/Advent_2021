import Data.List(foldl')
direction (horz, vert, aim) instruction = func word amt (horz, vert, aim)
    where [word, amtstr] = words instruction
          amt = read amtstr
          func wrd a (h, v, am) | wrd == "forward" = (h+a, v+(am*a), am)
                                | wrd == "up"      = (h, v, am-a)
                                | wrd == "down"    = (h, v, am+a)
                                | otherwise        = (h, v, am)
payload = foldl' direction (0, 0, 0)
main = do
    lis <- readFile "2_1.txt"
    print . uncurry (*) . (\(x1,x2,x3) -> (x1,x2)) .payload . lines $ lis