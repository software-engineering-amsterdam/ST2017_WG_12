import Data.List

-- taken from https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
toDigits :: Integer -> [Integer]
toDigits n = map (\x -> read [x] :: Integer) (show n)

-- taken from https://stackoverflow.com/questions/1918486/convert-list-of-integers-into-one-int-like-concat-in-haskell
toInt :: [Integer] -> Integer
toInt = foldl addDigit 0 where addDigit num d = 10*num + d

main = do
  putStrLn $ show $ [(x, x + 3330, x + 6660) | x <- [1..4000], elem (toDigits (x + 3330)) (permutations (toDigits x)), elem (toDigits (x + 6660)) (permutations (toDigits x))]
-- time taken: 5m
