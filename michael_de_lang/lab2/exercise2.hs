import Data.List
import System.Environment

data Shape = NoTriangle | Equilateral | Isoscele | Rectangular | Other deriving (Eq, Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | a < 0 || b < 0 || c < 0 = error "No imaginary triangles please"
               | a + b < c || a + c < b || b + c < a || a == 0 || b == 0 || c == 0 = NoTriangle
               | a == b && b == c = Equilateral
               | a == b || b == c || a == c = Isoscele
               | a^2 + b^2 == c^2 || b^2 + c^2 == a^2 || a^2 + c^2 == b^2 = Rectangular
               | otherwise = Other



main = do
    let nones = [(1,1,3), (2,5,2), (7, 3, 3), (0,0,0)]
    let equilaterals = [(1,1,1), (2,2,2), (3,3,3), (500,500,500)]
    let isosceles = [(1,1,2), (1,2,1), (2,1,1), (5,5,1), (5,1,5), (1,5,5)]
    let rectangulars = [(3,4,5), (3,5,4), (4,5,3), (5,13,12), (5,12,13)]
    let others = [(2,3,4), (3,2,4), (4,3,2)]
    putStrLn $ show $ all (\(a, b, c) -> triangle a b c == NoTriangle) nones
    putStrLn $ show $ all (\(a, b, c) -> triangle a b c == Equilateral) equilaterals
    putStrLn $ show $ all (\(a, b, c) -> triangle a b c == Isoscele) isosceles
    putStrLn $ show $ all (\(a, b, c) -> triangle a b c == Rectangular) rectangulars
    putStrLn $ show $ all (\(a, b, c) -> triangle a b c == Other) others

-- time so far: 30m
