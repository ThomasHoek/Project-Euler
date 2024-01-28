fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
-- generator of fib

evenListNumbers :: [Integer] -> [Integer]
evenListNumbers = filter even

main :: IO ()
main =  print $ (sum . evenListNumbers) $ takeWhile (<= 400000000) fibs
