zipself xs = xs `zip` tail xs
incr_amount xs = foldr (\(x, y) r -> if x < y then r+1 else r) 0 (zipself xs)
main = do
       n <- getStrInput
