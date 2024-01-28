import Data.List(find)
import Data.Maybe ( fromJust )
import Numeric ( showFFloat )


toTriple :: (Eq a, Floating a) => (a, a) -> [a]
toTriple (m, n) = genTriple m n 

genTriple :: (Eq a, Floating a) => a -> a -> [a]
genTriple m n
    | (a**2 + b**2) == (c**2) = [a, b, c]
    | otherwise = [0, 0 ,0]
    where a = m**2 - n**2
          b = 2*m*n
          c = m **2 + n **2

showFullPrecision :: Double -> String
showFullPrecision x = showFFloat Nothing x ""

main :: IO () 
main =  putStrLn $ showFullPrecision sol
    where sol = product $ fromJust ansPair
          ansPair = find (\x -> 1000 == sum x) [genTriple b a | a <- [1..1000], b <- [a+1..1002]]

    