import Data.List

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g []       = []
altMap f g (x:[])   = f x : []
altMap f g (x:y:xs) = f x : g y : altMap f g xs

luhnDouble :: Integer -> Integer
luhnDouble x = if n < 10 then n else n - 9
               where n = x*2

mod10NoRemainder :: Integral a => a -> Bool
mod10NoRemainder x = (mod x 10) == 0

luhn :: [Integer] -> Bool
luhn = mod10NoRemainder . sum . (altMap luhnDouble id)

luhnAmericanExpress :: [Integer] -> Bool
luhnAmericanExpress = mod10NoRemainder . sum . (altMap id luhnDouble)

-- taken from https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
toDigits :: Integer -> [Integer]
toDigits n = map (\x -> read [x] :: Integer) (show n)

-- taken from https://stackoverflow.com/questions/1918486/convert-list-of-integers-into-one-int-like-concat-in-haskell 
toInt :: [Integer] -> Integer
toInt = foldl addDigit 0 where addDigit num d = 10*num + d

isAmericanExpress :: Integer -> Bool
isAmericanExpress x = (firstTwo == 34 || firstTwo == 37) && length digits == 15 && luhnAmericanExpress digits where
  digits = toDigits x
  firstTwo = toInt $ take 2 digits

isMaster :: Integer -> Bool
isMaster x = ((firstTwo >= 51 && firstTwo <= 55) || (firstFour >= 2221 && firstFour <=2720)) && length digits == 16 && luhn digits where
  digits = toDigits x 
  firstTwo = toInt $ take 2 digits
  firstFour = toInt $ take 4 digits

isVisa :: Integer -> Bool
isVisa x = digits !! 0 == 4 && (length digits == 13 || length digits == 16 || length digits == 19) && luhn digits
  where digits = toDigits x

main = do
  let amExNumbers = [373880337659522,377661975930391,370701162614742,349352372863923,375684547926772,349813813922412,340801991391970,347227713429919,377693123451798,378370472971119,372792467814994,374347704665671,373796125847907,373719505757470,371983315150506,372442925729802,378761088197364,375448726177957,347037371661020,342854717755257,373306288377625,342211585475577,343235419902463,376546687823888,340273064581244,342450283948119,342256954114844,349115366366777,377654754141546,378606600233009,341307706114870,343380872740677,348812969536753,378306161052167,371838447299995,373674236619392,345655746263164,348286707091050,373836774757443,346149459313811,342415344945040,379602483932501,341312452516401,343129448230451,376189975125680,343679590775703,349819269009091,374955112234599,347614146045117,370740738438046,373373375175296,349817498043535,373117442625498,371264245645604,372503512233761,349160815410628,342008489695155,379950254675316,371341176890159,378850151972538,346763626907725,347126422452404,375389351543958,343082305362141,376780417994848,378375396294720,346421566536380,373491080805385,375991363129153,349902146247439,376165651713619,379445005082623,375195359659781,340112952438960,340323978393929,347341468881859,346221984975826,343358339513713,343463751856754,371350242042570,340790570449921,372773589068289,343917107175342,343888156173233,374769396202994,371448425333707,378831860414798,377375345776612,341936721521436,344422450175139,348092939556807,342247682049634,378865138661095,378509755088006,378315008336071,343464878634934,347314787271557,345195608810646,343823260827218,375648708635522]
  let masterNumbers = [5555555555554444, 5437291847566632, 5531580388608737, 5145693648201538, 5206784260722632]
  let visaNumbers = [4111111111111111, 4271706756598344, 4485585424322309, 4716463000295458, 4929165220142104]

  let correctValues = map isAmericanExpress amExNumbers
  if elem False correctValues then
    putStrLn "Error in american express checker"
  else
    putStrLn "American Express number checker passed test"

  let correctValues = map isMaster masterNumbers
  if elem False correctValues then
    putStrLn "Error in mastercard checker"
  else
    putStrLn "Mastercard number checker passed test"

  let correctValues = map isVisa visaNumbers
  if elem False correctValues then
    putStrLn "Error in visa checker"
  else
    putStrLn "Visa number checker passed test"

-- Time taken: 45m
-- I took the Haskell premaster, so I already had the luhn function readily available. Some guy put the answers on github: https://github.com/adrianwong/programming-in-haskell-2nd-edition/blob/master/ch07_solutions.hs
