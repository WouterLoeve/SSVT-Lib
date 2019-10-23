module ExamFuncs where
import Data.List
import Data.Char
import Data.Tuple
import Data.Function
import Test.QuickCheck
import Control.Monad
import Control.Conditional
import SetOrd
import System.Random
import Lecture3

{- 
 - General
 -}

genPositiveIntegers :: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (> 0)

{-
 - Test a prop with multiple (different) inputs. if they are the same you could just use vectorOf
 -}
testProps = quickCheck $ 
    forAll genPositiveIntegers $ \x  ->
    forAll (arbitrary :: Gen Float) $ \y ->
        prop_test testFunc x y 

{-
 - Test a prop with multiple (different) inputs if they are the same you could just use vectorOf
 - On multiple functions
 -}
testMultiple = mapM (\f -> quickCheck $ 
    forAll genPositiveIntegers $ \x  ->
    forAll (arbitrary :: Gen Float) $ \y ->
        prop_test f x y) [testFunc, testFunc2]

prop_test f x y = f x y < f x y+1

testFunc :: Integer -> Float -> Integer
testFunc x y = (ceiling y) + x

testFunc2 :: Integer -> Float -> Integer
testFunc2 x y = (round y) + x


-- Skeleton for an arbitrary generator
generalGenerator :: Gen Float
generalGenerator = abs <$> (arbitrary :: Gen Float) `suchThat` (\x -> x < 1 && x > 0)

{-
 - Can be used as a skeleton for recursive generation. Please improve if necessary.
 -}
arbForm' :: Integral a => a -> Gen Form
arbForm' 0 = fmap Prop (suchThat arbitrary (>0))
arbForm' n = frequency
    [ (1, fmap      Prop (suchThat arbitrary (>0))),
      (1, fmap      Neg param),
      (1, liftM2    Impl param param),
      (1, liftM2    Equiv param param),
      (1, fmap      Dsj (vectorOf 2 param)),
      (1, fmap      Cnj (vectorOf 2 param)) ]
    where param = arbForm' (n `div` 2)

instance Arbitrary Form where
    arbitrary = sized $ \ n -> arbForm' (round (sqrt (fromIntegral n)))

{-
 - Relations Lab4
 - Some of these functions were taken from the labs (group 2) & lecture code.
 - The rest was created on basis of the definitions in the haskel road to programming book.
 -}
type Rel a = [(a,a)]

isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial dom rel = all (\x -> or [(x, y) `elem` rel | y <- dom]) dom

isReflexive :: [Int] -> Rel Int -> Bool
isReflexive dom rel = all (\x -> (x, x) `elem` rel) dom

isSymmetric :: Rel Int -> Bool
isSymmetric r = and [(b, a) `elem` r | (a, b) <- r]

isTransitive :: Rel Int -> Bool
isTransitive r = and [(a,c) `elem` r | (a,b) <- r, (b',c) <- r, b' == b]

isIrreflexive :: [Int] -> Rel Int -> Bool
isIrreflexive dom r = all (\x -> and [(x, y) `notElem` r | y <- dom, x == y]) dom

isAsymmetric :: Rel Int -> Bool
isAsymmetric r = and [(b, a) `notElem` r | (a, b) <- r]

isAntisymmetric :: Rel Int -> Bool
isAntisymmetric r = and [a == b | (a, b) <- r, (b, a) `elem` r]

isIntransitive :: Rel Int -> Bool
isIntransitive r = and [(a,c) `notElem` r | (a,b) <- r, (b',c) <- r, b' == b]

isLinear :: [Int] -> Rel Int -> Bool
isLinear dom r = all (\b -> and [((a,b) `elem` r) || ((b,a) `elem` r) || (a == b) | a <- dom]) dom

isEquivalence :: [Int] -> Rel Int -> Bool
isEquivalence dom r = isReflexive dom r && isSymmetric r && isTransitive r

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a 
trClos r = sort $ fix (\ f s -> if s == unComp s then s else f $ unComp s) r
    where unComp s = s `union` (r @@ s)

symClos :: Ord a => Rel a -> Rel a
symClos r = sort $ r `union` (swap <$> r)

reflClos :: Ord a => [a] -> Rel a -> Rel a
reflClos dom r = r `union` [(x,x) | x <- dom]

equivClos :: [Int] -> Rel Int -> Rel Int
equivClos dom r = sort $ fix (\ f s -> if s == final s then s else f $ final s ) r
    where 
        final s = trClos s `union` symClos s `union` reflClos dom s

-- Assumes all elements of the domain are in the relation
getDom :: Ord a => Rel a -> [a]
getDom r = nub $ concatMap (\ (x, y) -> [x, y]) r


{-  
 - Equivalence partitioning:
 - Implementations of the bell numbers & stirling numbers of the second kind taken from solutions of the haskell road to programming.
 -}
bell :: Integer -> Integer
bell 0 = 1
bell n = sum [stirling n k | k <- [1..n]]
 
stirling :: Integer -> Integer -> Integer
stirling n 1 = 1
stirling n k | n == k = 1
             | otherwise = k * (stirling (n-1) k) + stirling (n-1) (k-1)

{-
 - Sets see lab 4
 -}

{-
 - Trees
 -}

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

grow :: (node -> [node]) -> node -> Tree node

grow step seed = T seed (map (grow step) (step seed))

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

mapT :: (a -> b) -> Tree a -> Tree b
mapT f (T a xs) = (T (f a) ((mapT f) <$> xs))

count' :: Tree a -> Int
count' = foldT (\_ xs -> 1 + sum xs)

depth' :: Tree a -> Int
depth' a = (foldT (\ _ xs -> foldl max 0 xs + 1) a) -1

{-
 - Boolean formula functions
 -}

contradiction, tautology :: Form -> Bool
contradiction = not . satisfiable
tautology f = all (`evl` f) (allVals f)

entails, equiv :: Form -> Form -> Bool
f `entails` g = tautology $ Impl g f
f `equiv` g = tautology $ Equiv f g