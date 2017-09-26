import Control.Monad
import System.Random
import SetOrd
import Data.List

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: (Ord a, Show a) => Rel a -> Rel a
trClos xs | applyTr == xs = xs
          | otherwise = trClos applyTr
          where applyTr = sort $ nub $ (xs ++ (xs @@ xs))

-- time spent: 30m