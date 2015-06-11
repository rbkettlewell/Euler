isPrime :: Int -> Bool
isPrime x = l == (r - 1)
    where l = length $ takeWhile (/=0) $ map (x `mod`) [2..r] 
          r = floor . sqrt . fromIntegral $ x
sumPrimesLessThanN :: Int -> Int
sumPrimesLessThanN n = foldl1 (+) . filter isPrime $ [2..(n-1)]
