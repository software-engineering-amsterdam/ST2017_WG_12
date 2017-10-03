import Control.Monad
import System.Random
import SetOrd
import Data.List

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos xs = sort $ nub $ (xs ++ (map (\(x,y) -> (y,x))) xs)

-- time spent: 15m