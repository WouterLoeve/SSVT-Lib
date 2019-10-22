module Lab5solutions where
import Data.List
import Data.Char
import Data.Tuple
import Data.Function
import Test.QuickCheck hiding (choose)
import Control.Monad
import Control.Conditional
import SetOrd
import System.Random
import Lecture5 hiding (composites)
import Debug.Trace
import Criterion.Main
import Numeric.SpecFunctions (choose, logChoose, log1p)

{-
 - testRunhelper helps print results
 -}
testRunHelper testName numCases numPass = do
    let numFail = numCases - numPass
    let prepend = if numFail > 0 then "--- FAIL" else "+++ OK"
    let append = if numCases == numPass then "" else " ("++ show numFail ++" Failed)"
    prepend ++ ", Test '" ++ testName ++ "' Passed " ++ show numPass ++ " out of " ++ show numCases ++ " cases" ++ append


{-
 - Exercise 1
 - exM implementation is in Lecture5.hs!
-}

{-
 - Checks if the answer corresponds to the naive implementation expM: x^y `rem` n
 -}
prop_checkPower :: [Integer] -> Bool
prop_checkPower [a, b, c] = exM a b c == expM a b c

{-
 - Check whether the end result is actually smaller than the modulus
 -}
prop_checkPowerMod :: [Integer] -> Bool
prop_checkPowerMod [a, b, c] = exM a b c < c

{-
 - Test data generator for simple positive numbers
 -}
genPositiveIntegers :: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (> 0)

{-
 - Test Exm implementation
 -}
testExm :: IO ()
testExm = do
    print "Testing whether our fast version yields the same output as expM"
    quickCheck $ forAll (vectorOf 3 genPositiveIntegers) prop_checkPower
    quickCheck $ forAll (vectorOf 3 genPositiveIntegers) prop_checkPowerMod

{-
 - Test data generation for random performance testing
 - This function generates a list of three Integers, representing the base, exponent and modulo
 - The size of the resulting numbers is determined by a sizing parameter
-}
sizedInts :: Integer -> IO [Integer]
sizedInts n = do
    a <- randomRIO (1, n)
    b <- randomRIO (1, n)
    c <- randomRIO (1, n)
    return [a,b,c]

{-
 - Benchmark Exm implementation for random data
 - We use the `defaultMainWith` method from Criterion to run our benchmarks.
 - This method expects a list of benchmarks, each encapsulated by an environment (the `env` function)
 - This environment makes sure the same randomly generated test data is given to all functions in the benchmark.
 - If the environment was not used, two functions in the same benchamrk could recieve different testdata,
 - and the comparison would no longer be fair. This method was sourced from:
 - https://www.stackbuilders.com/news/obverse-versus-reverse-benchmarking-in-haskell-with-criterion
 - One pitfall is the requirement that benchmark funcitons use lazy pattern matching. This is achieved by
 - adding a tilde `~` character before the pattern. also the use of the `whnf` function is required in order to
 - make the resulting structure lazy. Critirion requires this in order to evaluate the function only while being timed.
 -
 - So we wanted to test higher numbers, but the expM
    function would crash our laptops with a size of 10^10.
 - For low numbers in a range of (1,100) we noticed that the
    expM is an order of magnitude faster. We theorise that this has
    to do with the overhead of recursion on smaller numbers.
 - For numbers between (1, 10^5) we noticed that our exM function
    was about 3 times faster than than the expM function.
 - For numbers between (1, 10^8) we saw a huge speedup in our exM function
    compared to the expM function.
    The results varied from microseconds for our exM function versus
        miliseconds of our expM function.
 - To conclude we see that our function is slower for small values around the 100s.
 - We see a slight speedup in the larger numbers around 10^5.
 - We see several orders of magnitude speedup for values around 10^8
 - We would like to test higher values as well, but as we mentioned before
    this would take a long time and computers with good performance to test the expM function.
 - It would also be interesting to test the (memory) efficiency and do complexity estimation
    increasing the number of bits.
 - Another thing to mention is that our function is limited by the recursion limit.
-}
benchExm :: IO ()
benchExm = do
    print "Benchmarking exM vs expM performance using random input"
    defaultMainWith defaultConfig [
        env (sizedInts 100) (\ ~[a,b,c] ->
            bgroup "Benchmarking random input with size 100"
                [bench "exM" $ whnf (exM a b) c
                ,bench "expM" $ whnf (expM a b) c])
        ,env (sizedInts (10^5)) (\ ~[a,b,c] ->
            bgroup "Benchmarking random input with size 10^5"
                [bench "exM" $ whnf (exM a b) c
                ,bench "expM" $ whnf (expM a b) c])
        ,env (sizedInts (10^8)) (\ ~[a,b,c] ->
            bgroup "Benchmarking random input with size 10^8"
                [bench "exM" $ whnf (exM a b) c
                ,bench "expM" $ whnf (expM a b) c])]

{-
 - Exercise 2
 - Time: 120 min
 - A way of testing infinite lists is mentioned in the QuickCheck paper:
https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf
 - It says that two infinite lists are equal if all finite initial segments are equal.
 - This is not always the case, you can program in logic which makes the list stop working after a certain number
 - For example in a list comprehension, use an extra conditional to say that the number should be lower than some arbitrary number.
 - If that happens this way of testing doesn't work.
 - We can however, find some errors in the function looking at a initial finite part.
 -}

{-
 - A faster version, doesn't produce the elements in orderly fashion but is many times faster.
 -}
compositesUnordered :: [Integer]
compositesUnordered = nub [x * y | x <- [2..], y <- [2..x]]

{-
 - The set of natural numbers can be split into primes and non-primes
 - All natural numbers are divisible by 1 and itself
 - All prime numbers are ONLY divisible by 1 and itself
 - All composite numbers are divisible by MORE than 1 and itself
 - Because all prime numbers are ONLY divisible by 1 and itself, and all natural numbers
 - are divisible by 1 and itself, all non-primes must be divisible by MORE than 1 and itself
 - Therefore all non-primes are composite numbers
 - We can implement this in Haskell by simply taking the natural numbers and removing primes.
-}
composites :: [Integer]
composites = filter (not . prime) [4..]

{-
 - Tests whether the number of factors for the first n composites is higher than 1.
 - Each element in the list of factors is multiplied with each other, and the result is
 - multiplied with 1, resulting eventually in the initial given number.
 -
 - If the length of the factors is 1, this means that the list of factors only contains the
 - given number itself. Therefore, when the given number is a prime, the length of the
 - list of factors is 1.
 -
 - So we want to test whether the amount of factors of each composite is
 - larger than 1, which is stated as a property below.
 -
 - Finally, a property is added stating that the given composites should
 - be positive numbers or else they would not be composites of natural numbers (as stated in the exercise).
 -}
prop_compFactor :: Integer -> Bool
prop_compFactor n = all (>1) ((length . factors) <$> (take (fromIntegral n) composites))

prop_compPositive :: Integer -> Bool
prop_compPositive n = all (>0) (take (fromIntegral n) composites)

testComposites :: IO ()
testComposites = do
    print "Testing whether sample elements have factors other than 1 and itself"
    quickCheck $ forAll genPositiveIntegers prop_compFactor
    print "Testing whether sample elements are positive numbers"
    quickCheck $ forAll genPositiveIntegers prop_compPositive


{-
 - Exercise 3
 - In addition to the single trials, do some testing on the modes of the smallest composite numbers accepted for any k.
 - We hypothesize that the least composite number scales exponentially with k due to needing to pass all tests.
 - Early testing results seem to agree with this.
 -
 - Note that the test may take a long time due to nondeterminism.
 - The running time of the test should scale somewhat exponentially too.
 - This is due to the combination of more or larger: k's used, repeated trials,
 -  higher primes (in the composite generator), higher exponentiation.
 - Each aspect causes the others to increase as well.
 - Fairly conservative values were chosen, but it may still be necessary to restart the test in some cases.
 -}
leastComposite :: Int -> [Integer] -> IO Integer
leastComposite k = fix (\f (x:xs) -> do b <- primeTestsF k x; if b then return x else f xs)

{-
 - Calculates the mode, ergo, the most occuring value.
 - https://rosettacode.org/wiki/Averages/Mode#Haskell
 -}
mode :: (Ord a) => [a] -> [a]
mode xs = map fst $ filter ((==best).snd) counts
    where counts = map (\l -> (head l, length l)) . group . sort $ xs
          best = maximum (map snd counts)

modalComposites :: [Int] -> Int -> [Integer] -> IO [[Integer]]
modalComposites ks n l = mapM (\k -> mode <$> replicateM n (leastComposite k l)) ks

leastComposites :: IO ()
leastComposites = do
    leastComposite1 <- leastComposite 1 composites
    leastComposite2 <- leastComposite 2 composites
    leastComposite3 <- leastComposite 3 composites
    print $ "Least composite for k = 1 is " ++ show leastComposite1
    print $ "Least composite for k = 2 is " ++ show leastComposite2
    print $ "Least composite for k = 3 is " ++ show leastComposite3
    print "Modal least composites for ks = [1, 2, 3, 5, 7] for trials n = 10"
    print =<< modalComposites [1, 2, 3, 5, 7] 10 composites

{-
 - Exercise 4
 - Fermat's little theorem states a^(p-1) mod p = 1 for any prime p and integer a not divisible by p.
 - Carmichael numbers are numbers for which this is true for any a coprime to them.
 - Thus a carmichael number will fool the Fermat primality test if only coprime a's are picked.
 - The generated list is a subset of the carmichael numbers.
 -
 - We expect the carmichael numbers to fool the Fermat primality test most of the time (= if only coprimes are picked).
 - The proportion of this depends on the number of coprimes and amount of a's picked k.
 - The formula for this is ((ncoprimes n) / (n - 2))^k.
 - The base pass rate (k=1) of the first carmichael number 294409 is ~0.95%.
 - Thus we expect ~(0.95^k)% of least composites to be the first carmichael number.
 - In fact, we expect the first number to be most common until around k=14, since 0.95^k > 0.5 for 0 < k < 14.
-}
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
      k <- [2..],
      prime (6*k+1),
      prime (12*k+1),
      prime (18*k+1) ]

ncoprimes :: Integer -> Int
ncoprimes n = length $ filter (\m -> gcd n m == 1) [2..n-1]

{- The expected rate at which the nth carmichael number fools the Fermat test for k trials.
 - Note: please don't run this for any n > 0 (or maybe n = 1 if compiled).
 - We intend this to be used to check the proportion of first (zero-th) carmichaels passing the test.
-}
expectedPassRate :: Fractional a => Int -> Int -> a
expectedPassRate n k = (fromIntegral (ncoprimes m) / fromIntegral (m - 2)) ^ k
    where m = carmichael !! n

{- Source: https://hackage.haskell.org/package/statistics-0.15.0.0/docs/src/Statistics.Distribution.Binomial.html#binomial
 - Implements the binomial distribution formula to compute the p-value
 - which indicates the significance of our results.
-}
binomTest :: Int -> Double -> Int -> Double
binomTest n p k
  | k < 0 || k > n = 0
  | n == 0         = 1
  | n < 1000       = choose n k * p^k * (1-p)^(n-k)
  | otherwise      = exp $ logChoose n k + log p * k' + log1p (-p) * nk'
  where
    k'  = fromIntegral k
    nk' = fromIntegral $ n - k

{- We run the Fermat's primality test 1000 times on the first carmichael number,
 - and return the list of booleans from which each boolean tell us if the
 - number has fooled the primality test or not.
-}
testPrimalityCarmichael :: IO [Bool]
testPrimalityCarmichael = do
    successes <- sequence $ fmap (\_ -> primeTestsF 1 (head (take 1 carmichael))) [1..1000]
    return successes

leastComposites' :: IO ()
leastComposites' = do
    print "4.1"
    leastComposite1 <- leastComposite 1 carmichael
    leastComposite2 <- leastComposite 2 carmichael
    leastComposite3 <- leastComposite 3 carmichael
    print $ "Least composite carmichael number for k = 1 is " ++ show leastComposite1
    print $ "Least composite carmichael number for k = 2 is " ++ show leastComposite2
    print $ "Least composite carmichael number for k = 3 is " ++ show leastComposite3
    print "Modal least composites for ks = [1, 2, 3, 5, 7] for trials n = 10"
    print =<< modalComposites [1, 2, 3, 5, 7] 10 carmichael

    print "4.2"
    results <- testPrimalityCarmichael
    let n = 1000
    let p = 0.95
    let e = 950
    let a = 0.05
    let k = length (filter (==True) results)
    let d = abs e - k
    let pval = binomTest n p k / (sum $ binomTest n p <$> [e + d..n] ++ [0..e - d])
    print $ "observed times fooled: " ++ show k ++ " (expected " ++ show e ++ ")"
    print $ "two-tailed p-value for the Fermat's test with n = " ++ show n ++ ", p = 0.95: " ++ show pval
    print $ "correct with confidence " ++ show a ++ ": " ++ show (a > pval) 

    print "4.3"
    let leastComposite' k = fix (\f (x:xs) -> do b <- primeMR k x; if b then return x else f xs)
    leastComposite1' <- leastComposite' 1 carmichael
    leastComposite2' <- leastComposite' 2 carmichael
    leastComposite3' <- leastComposite' 3 carmichael
    print $ "Least composite carmichael number for MR test with k = 1 is " ++ show leastComposite1'
    print $ "Least composite carmichael number for MR test with k = 2 is " ++ show leastComposite2'
    print $ "Least composite carmichael number for MR test with k = 3 is " ++ show leastComposite3'

{-
 - Exercise 5
 - Mersenne primes are primes of the form 2^n - 1.
 - The generated numbers are of this form by definition, so we only have to check primality.
 - That is to say, the numbers generated by the probablyMPrimes function are all probable primes of the Mersenne form.
-}
probableMPrimes :: Int -> [Integer] -> Int ->  IO [Integer]
probableMPrimes 0 _ k = return []
probableMPrimes n (p:ps) k = do
    let mp = 2^p - 1
    b <- primeMR k mp
    if b then
        (mp :) <$> probableMPrimes (n - 1) ps k
    else
        probableMPrimes n ps k

mersprimes = [mers x | x <- [1..25]]

{-
 - With this function we print the first x primes.
 - Then for k, 0 to 4 we check the number of falsely generated primes for our function.
        In which k is the number of rounds used for the miller rabin primality check.
 - We found that the number of falsely generated primes decreases fast by increasing k.
 - If we set k = 0; we can see how often the property if p = prime, 2^p - 1 is also prime holds.
 - On a recent desktop we were able to generate 23 mersenne primes with k=1, excluding the error rate computation.
 -}
someMPrimes :: Int -> IO ()
someMPrimes x = do
    print ("First " ++ show x ++  " Mersenne primes obtained for k = 1")
    mprimes <- probableMPrimes x primes 1
    print mprimes
    let num = 1000
    rate <- checkForErrorRate x num 0 0
    print $ "Error rate over the first " ++ show x ++ " primes; k = 0: " ++ show rate ++ " / " ++ show (num*x)

    rate1 <- checkForErrorRate x num 0 1
    print $ "Error rate over the first " ++ show x ++ " primes; k = 1: " ++ show rate1 ++ " / " ++ show (num*x)

    rate2 <- checkForErrorRate x num 0 2
    print $ "Error rate over the first " ++ show x ++ " primes; k = 2: " ++ show rate2 ++ " / " ++ show (num*x)

    rate3 <- checkForErrorRate x num 0 3
    print $ "Error rate over the first " ++ show x ++ " primes; k = 3: " ++ show rate3 ++ " / " ++ show (num*x)

    rate4 <- checkForErrorRate x num 0 4
    print $ "Error rate over the first " ++ show x ++ " primes; k = 4: " ++ show rate4 ++ " / " ++ show (num*x)

{-
 - To check for fake mersenne primes we check the list of mersenne primes defined in the lecture code and
 - see whether our generated numbers are in that list.
 -}
checkForErrorRate :: Int -> Int -> Int -> Int -> IO Int
checkForErrorRate mer 0 errors k = return errors
checkForErrorRate mer x errors k = do
    mprimes <- probableMPrimes mer primes k
    let l = length $ filter (not . (`elem` mersprimes)) mprimes
    checkForErrorRate mer (x-1) (errors+l) k

{-
 - Exercise6
 - Time: 30 min
 -}
tree1 n = grow (step1 n) (1,1)
step1 n = \ (x,y) -> if x+y <= n then [(x+y,x),(x,x+y)] else [] -- step function

tree2 n = grow (step2 n) (1,1)
step2 n = \ (x,y) -> if x+y <= n then [(x+y,y),(x,x+y)] else [] -- step function


{-
 - Proof from workshop answers:
 - Euclids gcd algorithm has a base case of (1,1) for a coprime pair input.
 - The given tree generation function `step` does the reverse of euclid's GCD algorithm.
 - So if  supply a starting value of (1,1), we would end up generating all coprime pairs.
 - The explanation was already given in the answers to workhop 5.

 - Changing [(x+y,x),(x,x+y)] to [(x+y,y),(x,x+y)] changes only the symmetry from subtree-symmetric
 - to y-axis symmetric. Changing symmetry does not alter content, so tree1 and tree2 contain the same values
 - and are therefore both proven by the euclidan GCD proof.
 - That both trees contain the same values can be seen clearly in the following comparison:
 -                           ==Tree1==
 -                             (1,1)
 -             (2,1)-------------+-------------(1,2)
 -     (3,2)-----+-----(2,3)           (3,1)-----+-----(1,3)
 - (5,3)-+-(3,5)   (5,2)-+-(2,5)   (4,3)-+-(3,4)   (4,1)-+-(1,4)
 -
 -                           ==Tree2==
 -                             (1,1)
 -             (2,1)-------------+-------------(1,2)
 -     (3,1)-----+-----(2,3)           (3,2)-----+-----(1,3)
 - (4,1)-+-(3,4)   (5,3)-+-(2,5)   (5,2)-+-(3,5)   (4,3)-+-(1,4)
-}

{-
 - You can check that those pairs are in the tree by writing the following quickCheck property.
 - You can use the collect version used in the workshop to produce the pairs of the tree.
 - These can then be used to check against a list of genereted co-primes till n.
-}
prop_CoPrimeTree :: (Integer -> Tree (Integer, Integer)) -> Integer -> Bool
prop_CoPrimeTree tree n = sort (collect' (tree n)) == sort ([(x, y) | x <- [1..n], y <- [1..n], gcd x y == 1])

{-
 - From our example we learned that tree1 and tree2 are the same but mirrored.
 -}
prop_sameTree :: Integer -> Bool
prop_sameTree n = sort (collect' (tree1 n)) == sort (collect' (tree2 n))

{-
 - Produces a list of all tree nodes
 -}
collect' :: Tree a -> [a]
collect' = foldT (\ x xs -> x : concat xs)

{-
 - Fold for the tree data structure
 -}
foldT :: (a -> [b] -> b) -> Tree a -> b
foldT f (T x ts) = f x (map (foldT f) ts)

testTrees :: IO ()
testTrees = do
    print "Testing coprimality of tree1"
    quickCheck $ forAll genPositiveIntegers (prop_CoPrimeTree tree1)
    print "Testing coprimality of tree2"
    quickCheck $ forAll genPositiveIntegers (prop_CoPrimeTree tree2)
    print "Testing that tree1 and tree2 are equal in contents"
    quickCheck $ forAll genPositiveIntegers prop_sameTree

{-
 - Exercise 7
 - Time: 200 min
 - Source: https://en.wikipedia.org/wiki/RSA_(cryptosystem)
 - In this exercise we present our RSA implementation.
 - Instead of the given functions we implement our own encryption and decryption keys because we weren't
    satisfied with (partial lack of) random element they presented.
 - We test three properties:
    - Whether encrypting and decrypting in succession results in the original message.
    - Whether encryption doesn't just return the original message
    - Whether decryption doesn't just return the ciphertext.
 - We test these properties for a single key (so with multiple messages) using rsaTest
 - rsaTestMult tests the aforementioned function a number of times (see Main.hs).
 - The key length we use is 1024, since that keeps the algorithm runnable in reasonable time for multiple runs.
 - Another thing is that according to the wikipedia article above, RSA can be vulnerable to known plaintext attacks 
    if no padding is used. 
 - Our implementation does not feature padding. This would be future work.
 - Another thing that needs to be investigated is the security of this particular implementation of RSA.
-}

{-
 - Check whether two primes are of the same bitlength
 -}
sameBitLength :: Integer -> Integer -> Bool
sameBitLength a b = getBitLength a == getBitLength b

{-
 - Calculates the assumed length in bits (could be implementation specific)
 -}
getBitLength :: Integer -> Integer
getBitLength a = ceiling (logBase 2 (fromIntegral a))

{-
 - Calculates a random positive Integer within the range (a, b)
 -}
genRandomInt :: Integer -> Integer -> IO Integer
genRandomInt a b = do
    r <- randomRIO (a, b)
    return $ abs r

{-
 - Generates a prime that can be used in RSA based on an already generated prime p.
 - This function takes the suposed length of prime p into account when generating this second prime.
 -}
rsaPrime :: Integer -> IO (Integer, Integer)
rsaPrime p = do
        let len = (getBitLength p) - 1
        q <- getRsaPrime (2^len) (2^(len+1)-1)
        if sameBitLength p q then
            return (p, q)
        else
            rsaPrime p

{-
 - Generates a random RSA prime.
 - As an optimisation, if the number is even, it's not a prime so we throw it away and try again recursively.
 - We use the miller rabin test for 40 rounds to test whether this number is prime.
 - This number 40 comes from https://stackoverflow.com/questions/6325576/how-many-iterations-of-rabin-miller-should-i-use-for-cryptographic-safe-primes
 - Which calculates the number of rounds necessary to get to a safe confidence level that the number you have generated is actually a prime.
 -}
getRsaPrime :: Integer -> Integer -> IO Integer
getRsaPrime a b = do
    p <- genRandomInt a b
    if even p then
        getRsaPrime a b
    else do
        pPrime <- primeMR 40 p
        if pPrime then
            return p
        else
            getRsaPrime a b

{-
 - Generates two primes, the necessary keys and tests some properties of the RSA process.
 -}
rsaTest :: Integer -> IO ()
rsaTest bits = do
    p <- getRsaPrime 2 (2^bits)
    (p, q) <- rsaPrime p
    let n = p * q
    let tot = phi p q
    e <- genRSAencrypt tot
    let d = genRSAdecrypt e tot

    quickCheck (\v -> prop_encIsDecrypt v e d n)
    quickCheck $ forAll genPositiveIntegers (\v -> prop_encDoesSomething v e d n)
    quickCheck $ forAll genPositiveIntegers (\v -> prop_decDoesSomething v e d n)

{-
 - Randomly generates an encryption key for a given p and q.
 - We noticed that in the given lecture code the generation of this encryption key often yields the same value.
 - This results in the same message often being encrypted to the same ciphertext which is very insecure.
 - In this function this is counteracted by taking a random value.
 -}
genRSAencrypt :: Integer -> IO Integer
genRSAencrypt tot = do
    e <- randomRIO (1, tot)
    if gcd e tot == 1 then
        return e
    else
        genRSAencrypt tot

{-
 - Generates a decryption key given a totient value and an encryption key.
 - This is equal to the multiplicative inverse of e meaning that:
        e * d = 1 mod n
        with n being the totient function.
        e is the encryption key.
 -}
genRSAdecrypt :: Integer -> Integer -> Integer
genRSAdecrypt e tot = invM e tot

{-
 - Encoding and decoding in succession should yield the original message.
 -}
prop_encIsDecrypt :: Integer -> Integer -> Integer -> Integer -> Bool
prop_encIsDecrypt val e d n = val == rsaDecode (d, n) (rsaEncode (e, n) val)

{-
 - Checks whether encode actually changes the value.
 - There is a tiny chance that this property doesn't work as the encoding function for a
 - specific message might yield the same value as the original message.
 - This is usefull to test because an empty encrypt function followed by an empty decrypt
 - function would satisfy the prop_encIsDecrypt property.
 -}
prop_encDoesSomething :: Integer -> Integer -> Integer -> Integer -> Property
prop_encDoesSomething val e d n = val > 1 ==> rsaEncode (e, n) val /= val

{-
 - Same as the encDoesSomething but for decrypt.
 -}
prop_decDoesSomething :: Integer -> Integer -> Integer -> Integer -> Property
prop_decDoesSomething val e d n = val > 1 ==> rsaDecode (d, n) val /= val

{-
 - Tests the rsaTest function by calling it an x amount of times.
 - Ensures that we also use different keys for testing.
 - Also takes the number of bits used in computation.
 -}
rsaTestMult :: Integer -> Integer -> IO ()
rsaTestMult 0 bits = print "Done"
rsaTestMult x bits = do
    print ("Testing rsa properties with number of bits: " ++ show bits ++ " with " ++ show x ++ " runs left")
    rsaTest bits
    rsaTestMult (x-1) bits
