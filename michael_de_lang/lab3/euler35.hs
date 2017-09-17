prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

primesBelow :: Integer -> [Integer]
primesBelow 0 = []
primesBelow x = takeWhile (< x) primes

-- https://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

makeRots :: Integer -> [Integer]
makeRots xs = makeRots' xs (length (show xs) - 1)

makeRots' :: Integer -> Int -> [Integer]
makeRots' xs 0 = [xs]
makeRots' xs n = (read (rotate n (show xs))) : makeRots' xs (n - 1)

e35 = filter e35' (primesBelow (1000000))

e35' :: Integer -> Bool
e35' n = prime n && and (map (\x -> prime x) (makeRots n))

main = do
    let result = e35
    print $ e35
    print $ length e35
