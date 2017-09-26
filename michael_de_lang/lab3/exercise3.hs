import Data.List
import Lecture3

-- Mathematically multiply x with y in various cases. 
-- e.g. 1 v (2 ^3) becomes (1 v 2) ^ (1 v 3)
combineF :: Form -> Form -> Form
combineF (Prop x) (Cnj fs) = Cnj (map (\f -> Dsj [Prop x, f]) fs)
combineF (Cnj fs) (Prop x) = Cnj (map (\f -> Dsj [Prop x, f]) fs)
combineF (Prop x) (Dsj fs) = Dsj (map (\f -> Dsj [Prop x, f]) fs)
combineF (Dsj fs) (Prop x) = Dsj (map (\f -> Dsj [Prop x, f]) fs)
combineF (Cnj fs) (Cnj xs) = Cnj (concat (map (\f -> (map (\x -> Dsj [f, x]) xs)) fs))
combineF (Cnj fs) (Dsj x) = Cnj (map (\f -> Dsj [Dsj x ,f]) fs)
combineF (Dsj x) (Cnj fs) = Cnj (map (\f -> Dsj [Dsj x, f]) fs)
combineF x y = Dsj [x, y]

-- Find cases where a disjunction contains one or two conjunctions.
distributeORs :: Form -> Form
distributeORs (Prop x) = Prop x
distributeORs (Neg x) = Neg (distributeORs x)
distributeORs (Dsj [(Cnj p), (Cnj q)]) = distributeORs $ combineF (Cnj p) (Cnj q)
distributeORs (Dsj [p, (Cnj q)]) = distributeORs $ combineF p (Cnj q)
distributeORs (Dsj [(Cnj p), q]) = distributeORs $ combineF (Cnj p) q
distributeORs (Cnj xs) = Cnj (map distributeORs xs)
distributeORs (Dsj xs) = Dsj (map distributeORs xs)

-- This function recurses into itself until the outcome doesn't change anymore.
-- This is because distributeORs doesn't deal with two-level deep possibilities, that would be an insane
-- Acceptable input is only valid while the length of elements in a Dsj or Cnj is exactly 2.
cnf:: Form -> Form
cnf f | outcome /= f = cnf outcome
      | otherwise = outcome
      where outcome = (distributeORs . nnf . arrowfree) f

main = do
    print $ cnf $ head $ parse "+(1 *(2 +(3 *(4 5))))"

-- time taken: 480m

-- The amount of time taken is because we tried to make a solution for disjunctions and conjunctions with N properties, rather than 2. However, it took too long, so I ended up only implementing a solution for 2 properties.
