import Data.List

-- taken from https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
toDigits :: Integer -> [Integer]
toDigits n = map (\x -> read [x] :: Integer) (show n)

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]


main = do
  putStrLn $ show $ [(x, x + 3330, x + 6660) | x <- (takeWhile (< 4000) primes), elem (toDigits (x + 3330)) (permutations (toDigits x)), elem (toDigits (x + 6660)) (permutations (toDigits x)), prime (x + 3330), prime (x + 6660)]
-- time taken: 10m
-- This gives back two results: one tuple that is given in the problem and another tuple that is the other sequence the problem is talking about. Concatenated, this gives 296962999629.
