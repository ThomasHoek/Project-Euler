import Math.NumberTheory.Primes.Counting
-- arithmoi


nthPrime2  = sieve [2..]
sieve (p : xs) = p : sieve [ x | x <- xs, x `mod` p > 0 ]

main :: IO ()
main = do {
    print $ nthPrime 10001 ;
    print $ last $ take 10001 nthPrime2;
} 
