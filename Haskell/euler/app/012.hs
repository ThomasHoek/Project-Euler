import Data.List(nub, find)

triangle :: (RealFrac a, Integral b) => a -> b
triangle n = round $ n*(n+1)/2

-- | generate prime factors of a integer
factors :: Int -> [Int]
factors m = f m (head primes) (tail primes)
f m n ns
    | m < 2 = []
    | m < n ^ 2 = [m]
    | m `mod` n == 0 = n : f (m `div` n) n ns
    | otherwise = f m (head ns) (tail ns)

-- | prime sequence
primes :: [Int]
primes = 2 : filter (\n-> head (factors n) == n) [3,5..]

-- | count how often value x occurs in xs
countOcc :: Eq a => a -> [a] -> Int
countOcc x = length . filter (==x)

-- | find the powers and add them by 1
factors2Divisors :: Eq a => [a] -> [Int]
factors2Divisors n = [1 + countOcc c n | c <- nub n]

main :: IO ()
main = print sol
    where inp = map triangle [1..]                      -- infinite triangle list
          sol = find (\x -> 499 <= divNum x) inp        -- find solution
          divNum = product . factors2Divisors . factors -- bool statement