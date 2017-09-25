import Control.Monad
import System.Random
import SetOrd
import Test.QuickCheck
import Test.QuickCheck.Monadic

getValuesFromSet :: Set Int -> [Int]
getValuesFromSet (Set a) = a

setIntersection :: Set Int -> Set Int -> Set Int
setIntersection a b = do
    let as = getValuesFromSet a
    let bs = getValuesFromSet b
    list2set $ filter (\x -> elem x bs) as

setUnion :: Set Int -> Set Int -> Set Int
setUnion a b = do
    let as = getValuesFromSet a
    let bs = getValuesFromSet b
    list2set $ (as ++ bs)

setDifference :: Set Int -> Set Int -> Set Int
setDifference a b = do
    let as = getValuesFromSet a
    let bs = getValuesFromSet b
    list2set $ filter (\x -> notElem x as || notElem x bs) (as ++ bs)
