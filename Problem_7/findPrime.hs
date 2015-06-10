isPrime :: Int -> Bool
isPrime x = (x - l)  == 2
    where l = length $ takeWhile (/=0) $ map (x `mod`) [2..(x-1)]

findPrime :: Int -> Int
findPrime n = last $ take n $ filter isPrime [1..]
