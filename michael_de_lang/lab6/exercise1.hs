import Lecture6

highestPower :: Integer -> Integer -> Integer -> Integer
highestPower x x' y | x' > y = x
                    | otherwise = highestPower x' (x' * 2) y

exM'' :: Integer -> Integer -> Integer -> Integer
exM'' x y z | y == 1 = rem x z
            | otherwise = rem ((exM' x halfSquared z) * (exM' x halfSquared z)) z
    where halfSquared = quot y 2
                    
exM' :: Integer -> Integer -> Integer -> Integer
exM' x y z  | y == 1 = rem x z
            | y - squared == 0 = rem ((exM'' x halfSquared z) * (exM'' x halfSquared z)) z
            | otherwise = rem ((rem ((exM'' x halfSquared z) * (exM'' x halfSquared z)) z)  * (rem (x^(y-squared)) z)) z
    where squared = highestPower 1 1 y
          halfSquared = quot squared 2
          
main = print $ exM' 32 1911123 917

--time taken 90m