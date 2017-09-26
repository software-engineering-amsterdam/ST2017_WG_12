--output:
-- *Main> trClos $ symClos [(1,2),(2,3),(3,4)]
-- [(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(2,3),(2,4),(3,1),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4)]
-- *Main> symClos $ trClos [(1,2),(2,3),(3,4)]
-- [(1,2),(1,3),(1,4),(2,1),(2,3),(2,4),(3,1),(3,2),(3,4),(4,1),(4,2),(4,3)]

-- The transitive closure of a symmetric closure ends up generating a reflexive closure as well.
-- But this does not happen if you reverse the functions.