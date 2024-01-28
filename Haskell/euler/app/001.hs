import Data.List (nub)

problem1 :: (Eq a, Num a, Enum a) => a -> [a]
problem1 x = nub $ [3,6..x] ++ [5,10..x]

main :: IO ()
main = print $ (sum . problem1) 999
