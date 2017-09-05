-- 1.
-- This is exactly the same as the khan video I just watched.
-- So the base case is S(1) = 1. (1*(1+1))/2 = 1. So the base case is correct.
-- The proof is substituting p in the formula with (p+1). That gives us (p+1)*((p+1)+1) / 2.
-- Given that 1 is proven, filling in (1+1) should give us 3. (1+1)*((1+1)+1) / 2 = 3. This shows that this formula holds for all positive integers.

-- 2.
-- We did this in class.
-- The base case is 0^2 = 0*(0+1)(2*0+1)/6 = 0.
-- n*(n+1)*(2*n+1)/6 + (n+1)^2 = 2*(n+1)*(2*n+1)
-- n*(n+1)*(2*n+1)/6 + (n+1)^2 = 2*(n+1)*(2*n+1) 

-- 3.
-- Also in class.
-- Base case: 0 = 0
-- 1^3 + ... + n^3 + (n+1)^3 = (n*(n+1)/2)^2
-- (n*(n+1)/2)^2 + (n+1)^3      | * 4
-- (n*(n+1)/2)^2 


-- 4.
-- |An| = n, |P(An)| = 2^n
-- |An+1| = n+1, |P(An+1)| = 2^(n+1)
-- |P(An+1)| = |P(An union {n+1}| => {n+1} element P(An)
--                                   {n+1} not element P(An)
-- 2^n*2

-- 5. 
-- P(n) = n!; P(n+1) = (n+1)!; {1,2,3} + 4 = 4 different perms.

-- 6. 
-- 3^(2n+3) + 2^n; base case = 0; 3^3 + 2^0 = 28; 28 is divisible by 7.
-- 3^(2(n+1)+3) + 2^(n+1) = 3^(2n+2+3) + 2^(n+1) = 3^(2n+3)*3^2 + 2^n*2^1 = 3^(2n+3)*9 +2^n*2 = 3^(2n+3)*(7+2)+2^n*2 = 3^(2n+3)*7+3^(2n+3)*2+2^n*2
-- 3^(2n+3)*7+3^(2n+3)*2+2^n*2 = 3^(2n+3)*7+2(3^(2n+3)+2^n)
-- First part (*7) is divisible by 7 by definition. Let's call it R*7.
-- Second part is the same form for the induction hypothesis. Let's call the hypothesis 7*K.
-- That gives us R*7+2*7*K = 7*(R+2*K)

-- 7.
-- n^2 > 2*n
-- base case = 3; 3^2 > 2*3
-- (n+1)^2 > 2*(n+1) = n^2+2n+2 > 2n+2
-- 


-- 8.
-- 2^n > n^2
-- base case = 5; 2^5 > 5^2; 32 > 25;
-- 2^(n+1) > (n+1)^2; 2^n * 2^1 > n^2+2n+1;  | /2 
-- 2*2^n > 

-- 9.  
-- K matches = 4N
-- Other player took 1, 2 or 3 matches. So we're at 4N-1, or 4N-2 or 4N-3.
-- At 4N-1, I take -3, at 4N-2, I take -2, at 4N-3, I take -1.
-- So make sure that I always leave the system in 4(N-1) after my move.
-- 

-- 10. 
-- C(phi) = count connectives.
-- C(proposition) = 0
-- C(not phi) = 1 + C(phi)
-- C(phi ^ phi) = 1 + C(phi) + C(phi)
-- etc

-- 11.
-- same as 10, except C(prop) = 1 and get rid of the +1 everywhere.

-- 12.
-- 


-- 13.
-- n = 0 -> 0
-- n = 1 -> 1
-- n = 2 -> 2
-- n = 3 -> 3
-- Teken diagrammetje + uitleggen in woorden dat er met 1 extra interne node een nieuwe boom met diepte n+1 ontstaat.

--



-- extra voorbeelden
-- base case -> invullen en kijken of waar
-- 
