main = do
  putStrLn $ show $ [(x,y,z) | x <- [1..1000], y <- [1..1000], z <- [1..1000], x+y+z == 1000, x^2 + y^2 == z^2]
-- time taken: 2m
