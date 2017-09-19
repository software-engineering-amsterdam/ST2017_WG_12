import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

main = do
    print $ (parse "*(2 3)") == ([Cnj [Prop 2, Prop 3]])
    print $ (parse "-*(2 3)") == ([Neg (Cnj [Prop 2, Prop 3])])
    print $ (parse "+(2 *(3 -4))") == ([Dsj [Prop 2, Cnj [Prop 3, Neg (Prop 4)]]])

-- I have yet to figure out how to give a more comprehensive test case.

-- time taken: 30m

-- oipo@sd-59673:~/uva/software_testing/STS1/michael_de_lang/lab3$ ghc -O2 exercise2.hs && ./exercise2
-- [2 of 2] Compiling Main             ( exercise2.hs, exercise2.o )
-- Linking exercise2 ...
-- True
-- True
-- True
