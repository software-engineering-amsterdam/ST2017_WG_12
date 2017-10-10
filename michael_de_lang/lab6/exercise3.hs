import Lecture6 hiding (composites)

composites :: [Integer]
composites = [x | x <- [2..], not (prime x)]

-- time taken: 5m