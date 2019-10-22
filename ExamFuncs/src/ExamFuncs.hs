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

{- 
 - General
 -}

genPositiveIntegers :: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (> 0)





{-
 - Relations Lab4
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


{-  Wat missen we hier?
 - Equivalence partition ding without singletons
 -}

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

