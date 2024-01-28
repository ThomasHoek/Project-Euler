primeFactors :: Integral t => t -> [t]
-- https://stackoverflow.com/questions/21276844/prime-factors-in-haskell
primeFactors n =
  case factors of
    [] -> [n]
    _ -> factors ++ primeFactors (n `div` head factors)
  where
    factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n -1]

main :: IO ()
main = print $ (maximum . primeFactors) 600851475143