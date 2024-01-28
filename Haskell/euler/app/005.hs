lcmm :: Integral a => [a] -> a
lcmm = foldr lcm 1

main :: IO ()
main = (print .  lcmm) [1 .. 20]
