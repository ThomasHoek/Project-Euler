factors :: Int -> [Int]
factors m = f m (head primes) (tail primes)

f :: Integral a => a -> a -> [a] -> [a]
f m n ns
    | m < 2 = []
    | m < n ^ 2 = [m]
    | m `mod` n == 0 = n : f (m `div` n) n ns
    | otherwise = f m (head ns) (tail ns)

-- | prime sequence
primes :: [Int]
primes = 2 : filter (\n-> head (factors n) == n) [3,5..]

main :: IO () 
main = print $ sum $ takeWhile (<2000000) primes

    