reverseInt :: Integer -> Integer
reverseInt = read . reverse . show

isPalindrome :: Integer -> Bool
isPalindrome w = w == reverseInt w

palindroneLoop2 :: Integer -> Integer -> Integer
palindroneLoop2 _ 100 = 0
palindroneLoop2 n m = if isPalindrome x then x else palindroneLoop2 n (m - 1) where x = m * n

main :: IO ()
-- main = (print . maximum ) [palindroneLoop c | c <- reverse [1000 .. 9999]]
-- 54 sec

main = (print . maximum ) [palindroneLoop2 c 9999| c <- reverse [1000 .. 9999]]
-- 37 sec