import Lecture6 hiding(composites)
import Debug.Trace

-- Exercise 1
-- Time spent: 45 Min
exM' :: Integer -> Integer -> Integer -> Integer
exM' x y n = fst (e1 x y n)

e1 :: Integer -> Integer -> Integer -> (Integer,Integer)
e1 x y n | y == 0 = (rem 1 n, 1)
         | y == 1 = (rem x n, x)
         | otherwise = (rem ((fst z) * (rem (snd z) n)) n, p)
         where 
              z = (e1 x (div y 2) n)
              p = (snd z) * (snd z)


exerciseOne = do
                print "Running exM 2342 32454232 6"
                print "  Created exM:"
                print ("  " ++ show (exM' 2342 32454232 6))
                print "Origional exM:"
                print ("  " ++  show (exM 2342 32454232 6))


-- Exercise 2:
-- Time spent: 30 Min

-- Used values for exM:
-- 2342 32454232 6
-- Same as exercise one

-- Origional results:
--	Mon Oct 09 15:32 2017 Time and Allocation Profiling Report  (Final)
--
--	   Lab 6.exe +RTS -p -RTS
--
--	total time  =        5.24 secs   (5239 ticks @ 1000 us, 1 processor)
--	total alloc = 131,362,448 bytes  (excludes profiling overheads)
--
--COST CENTRE MODULE   SRC                   %time %alloc
--
--expM        Lecture6 Lecture6.hs:111:1-20  100.0  100.0
--
--
--                                                                                 individual      inherited
--COST CENTRE  MODULE                   SRC                     no.     entries  %time %alloc   %time %alloc
--
--MAIN         MAIN                     <built-in>               48          0    0.0    0.0   100.0  100.0
-- CAF         GHC.IO.Encoding.CodePage <entire-module>          80          0    0.0    0.0     0.0    0.0
-- CAF         GHC.IO.Encoding          <entire-module>          76          0    0.0    0.0     0.0    0.0
-- CAF         GHC.IO.Handle.Text       <entire-module>          67          0    0.0    0.0     0.0    0.0
-- CAF         GHC.IO.Handle.FD         <entire-module>          61          0    0.0    0.0     0.0    0.0
-- CAF         Main                     <entire-module>          56          0    0.0    0.0   100.0  100.0
--  main       Main                     Lab 6.hs:(18,1)-(19,43)  96          1    0.0    0.0   100.0  100.0
--   exM       Lecture6                 Lecture6.hs:114:1-10     99          0    0.0    0.0   100.0  100.0
--    expM     Lecture6                 Lecture6.hs:111:1-20    100          1  100.0  100.0   100.0  100.0
-- CAF         Lecture6                 <entire-module>          55          0    0.0    0.0     0.0    0.0
--  exM        Lecture6                 Lecture6.hs:114:1-10     98          1    0.0    0.0     0.0    0.0
-- main        Main                     Lab 6.hs:(18,1)-(19,43)  97          0    0.0    0.0     0.0    0.0
--

-- Results of exercise one:
--	Mon Oct 09 15:31 2017 Time and Allocation Profiling Report  (Final)
--
--	   Lab 6.exe +RTS -p -RTS
--
--	total time  =        0.61 secs   (610 ticks @ 1000 us, 1 processor)
--	total alloc =  23,526,376 bytes  (excludes profiling overheads)
--
--COST CENTRE MODULE SRC                %time %alloc
--
--e1.p        Main   Lab 6.hs:15:15-35   99.3   99.8
--
--
--                                                                                 individual      inherited
--COST CENTRE  MODULE                   SRC                     no.     entries  %time %alloc   %time %alloc
--
--MAIN         MAIN                     <built-in>               48          0    0.0    0.0   100.0  100.0
-- CAF         GHC.IO.Encoding.CodePage <entire-module>          80          0    0.2    0.0     0.2    0.0
-- CAF         GHC.IO.Encoding          <entire-module>          76          0    0.0    0.0     0.0    0.0
-- CAF         GHC.IO.Handle.Text       <entire-module>          67          0    0.0    0.0     0.0    0.0
-- CAF         GHC.IO.Handle.FD         <entire-module>          61          0    0.0    0.1     0.0    0.1
-- CAF         Main                     <entire-module>          56          0    0.0    0.0    99.8   99.8
--  main       Main                     Lab 6.hs:(18,1)-(19,44)  96          1    0.0    0.0    99.8   99.8
--   exM'      Main                     Lab 6.hs:7:1-27          98          1    0.0    0.0    99.8   99.8
--    e1       Main                     Lab 6.hs:(10,1)-(15,35)  99         25    0.5    0.0    99.8   99.8
--     e1.z    Main                     Lab 6.hs:14:15-36       100         24    0.0    0.0     0.0    0.0
--     e1.p    Main                     Lab 6.hs:15:15-35       101         23   99.3   99.8    99.3   99.8
-- main        Main                     Lab 6.hs:(18,1)-(19,44)  97          0    0.0    0.0     0.0    0.0
--

-- Exercise 3
-- Time spent: 10 Min
composites :: [Integer]
composites = [x|x<-[1..], length(factors x) >1]

limitComposites :: Integer -> [Integer]
limitComposites n = [x | x <- [1..n], (length $ factors x) > 1]

-- Exercise 4
-- Time spent: 15 Min - used iOAny of lastweek (10h)
exerciseFour :: Int -> IO Integer
exerciseFour k = primeCheck composites (primeTestsF k)

primeCheck :: [Integer] -> (Integer -> IO Bool) -> IO Integer
primeCheck xs f = (iOAny f xs)

iOAny :: (Integer -> IO Bool) -> [Integer] -> IO Integer
iOAny f (x:xs) = do
                  y <- f x
                  case y of
                    True -> return (x)
                    False -> iOAny f xs
iOAny f [] = do return (-1) -- Not found, impossible for the infinite list

-- When K is increased, so is (on average) the first fool:
-- K: 
--  0: 4
--  1: 15
--  2: 21
--  3: 703
--  4: 1387
--  5: 1729
--  6: 2465
-- K determinse the 'thoughness' of the test

-- Exercise 5
-- Time spent: 5 Min

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
    k <- [2..],
    prime (6*k+1),
    prime (12*k+1),
    prime (18*k+1) ]

exerciseFive :: Int -> IO Integer
exerciseFive k = primeCheck carmichael (primeTestsF k)

-- Exercise 6 (the first one)
exerciseSixF :: Int -> IO Integer
exerciseSixF k = primeCheck carmichael (primeMR k)

-- Exercise 6 (the second one)]
-- time spent: 10 Minutes
exerciseSixS :: Int -> Integer -> IO Bool
exerciseSixS k p = primeMR k (mers p)

