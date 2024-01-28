import Text.Printf ( printf )

sumSquare :: [Double] -> Double
sumSquare = (**2) . sum

squareSum :: [Double] -> Double
squareSum = sum . map (**2)

main :: IO ()
main = printf "%f\n" (sumSquare inp - squareSum inp)
    where inp = [1 .. 100]
