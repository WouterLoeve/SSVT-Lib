module Exam
where
import Test.QuickCheck
import ExamFuncs

-- type Rel a = [(a,a)]
isIrreflexive :: Eq a => Rel a -> Bool
isIrreflexive = error "not yet implemented"

fib :: Int -> Int
fib n = f n 1 1 where
 f 0 n1 n2 = n1
 f n n1 n2 = f (n-1) n2 (n1+n2)
 
freeLadybug1 u d totalD = let v = 1 - u in
 u^(totalD-d) * sum [ u^j * v^(d-j-1) | j <- [0..d-1] ] 
 / sum [ u^j * v^(totalD-j-1) | j <- [0..totalD-1] ]

freeLadybug2 u d totalD = let r = (1-u)/u in
 sum  [ r^j | j <- [0..d-1] ] / sum [ r^j | j <- [0..totalD-1]]

freeLadybug3 u d totalD = let v = 1 - u in
 (1 - (u/v)^d)/(1-(u/v)^totalD)

freeLadybug4 u d totalD = let v = 1 - u in
    v^(totalD-d)*(v^d-u^d)/(v^totalD-u^totalD)

freeLadybug5 u d totalD = let v = 1 - u in
    v^(totalD-d) * sum [ u^j * v^(d-j-1) | j <- [0..d-1] ]
    / sum [ u^j * v^(totalD-j-1) | j <- [0..totalD-1] ]

ownFreeLadybug u d totalD = let v = 1 - u in
    (1 - (v/u)^d)/(1-(v/u)^totalD)

test = do
    quickCheck $ forAll (vectorOf 2 genPositiveIntegers) (prop_ProbIncreases freeLadybug1)
    quickCheck $ forAll (vectorOf 2 genPositiveIntegers) (prop_ProbIncreases freeLadybug2)
    quickCheck $ forAll (vectorOf 2 genPositiveIntegers) (prop_ProbIncreases freeLadybug3)
    quickCheck $ forAll (vectorOf 2 genPositiveIntegers) (prop_ProbIncreases freeLadybug4)
    quickCheck $ forAll (vectorOf 2 genPositiveIntegers) (prop_ProbIncreases freeLadybug5)

    print $ show $ map (\ f -> f 0.5 3 4) [freeLadybug1, freeLadybug2, freeLadybug3, freeLadybug4, freeLadybug5]
    
    print "other test"
    mapM (\ f -> quickCheck $ 
        forAll genPositiveIntegers $ \d  ->
        forAll genU $ \u ->
            prop_Disd f u d) [freeLadybug1, freeLadybug2, freeLadybug3, freeLadybug4, freeLadybug5]

    print "0.5"

    mapM (\ f -> quickCheck $ 
        forAll (vectorOf 2 genPositiveIntegers) (prop_Ratio f) ) [freeLadybug1, freeLadybug2, freeLadybug3, freeLadybug4, freeLadybug5]

    print "Change u\n"
    mapM (\ f -> quickCheck $ 
        forAll (vectorOf 2 genPositiveIntegers) $ \input ->
        forAll genU $ \u ->
            prop_ProbIncreasesU f u input) [freeLadybug1, freeLadybug2, freeLadybug3, freeLadybug4, freeLadybug5]

genU :: Gen Double
genU = abs <$> (arbitrary :: Gen Double) `suchThat` (\x -> x < 1 && x > 0)

-- prop_Ratio :: (Fractional a, Integral b, Eq a, Eq b) => (a -> b -> b -> a) -> b -> b -> Bool
prop_Ratio f input = f 0.5 d totalD == (realToFrac d / realToFrac totalD)
    where d = minimum input
          totalD = maximum input

-- if d = D, prob should be 1, regardless of u.
-- Probability of the ladybug winning should go up if d increases.
-- If u == 0.5, d < D, answer is d / D

-- prop_Disd :: (Fractional a, Integral b) => (a -> b -> b -> a) -> a -> b -> Bool
prop_Disd f u d = f u d d == 1

prop_ProbIncreases :: (Fractional a, Integral b) => (a -> b -> b -> a) -> [Integer] -> Bool
prop_ProbIncreases f input = freeLadybug1 0.5 d totalD > freeLadybug1 0.5 d (totalD+1)
    where d = minimum input
          totalD = maximum input

-- The precondition is due to floating point errors.
prop_ProbIncreasesU f u input = d < totalD && f (u+0.1) d totalD < 0.9 ==> f (u+0.1) d totalD > f u d totalD
    where d = minimum input
          totalD = maximum input

-- Implementation 1 & 2 are correct according to these properties.