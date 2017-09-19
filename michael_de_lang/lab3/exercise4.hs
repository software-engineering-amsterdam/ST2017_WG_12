import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Control.Monad

-- inspiration http://www.cse.chalmers.se/~rjmh/QuickCheck/manual_body.html#16
form' :: Int -> Gen Form
form' 0 = liftM Prop arbitrary
form' n | n > 0 = oneof [liftM Prop arbitrary, liftM Neg subform,
                        liftM Cnj (vectorOf 2 subform), liftM Dsj (vectorOf 2 subform),
                        liftM2 Impl subform subform, liftM2 Equiv subform subform]
            where subform = form' (div n 2)


testGen = sized form'


main = quickCheck $ forAll testGen (\x -> satisfiable x)
