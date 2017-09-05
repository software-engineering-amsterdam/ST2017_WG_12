import Data.List

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses Matthew x = not (x==Matthew) && not (x==Carl)
accuses Peter   x = x==Matthew || x==Jack
accuses Jack    x = not (accuses Matthew x) && not (accuses Peter x)
accuses Arnold  x = accuses Matthew x /= accuses Peter x
accuses Carl    x = not (accuses Arnold x)

accusers x = [y | y <- boys, accuses y x]
guilty = [x | x <- boys, length (accusers x) == 3]
honest = [x | x <- boys, y <- guilty, accuses x y]

main = do
  putStrLn ("Guilty = " ++ (show guilty))
  putStrLn ("Honest = " ++ (show honest))

-- time taken 45m
