addorsub :: Int -> Int
addorsub x
    | x == 0 = 0
    | x `mod` 2 == 0 = x + addorsub (x - 1)
    | otherwise = subtract x (addorsub (x - 1))
