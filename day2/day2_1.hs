import Data.List(foldl')
direction (horz, vert) instruction = func word amt (horz, vert)
    where [word, amtstr] = words instruction
          amt = read amtstr
          func wrd a (h, v) | wrd == "forward" = (h+a, v)
                            | wrd == "backward"= (h-a, v)
                            | wrd == "up"      = (h, v-a)
                            | wrd == "down"    = (h, v+a)
                            | otherwise        = (h,v)
payload = foldl' direction (0, 0)
main = do
    lis <- readFile "2_1.txt"
    print . uncurry (*) . payload . lines $ lis