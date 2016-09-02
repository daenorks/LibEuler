import Data.List
import Data.Fixed

listcollatzB x = listcollatz2  x []

listcollatz2:: Integer -> [Integer] -> [Integer]
listcollatz2 x list
	| x == 1	= 1:list
	| even x	= listcollatz2 (div x 2) (x:list)
	| otherwise	= listcollatz2 (x* 3 +1) (x:list)

numdiv n = product $ map (+1) $ map length $ group $ primeF n

numdiv' n =  product $ map (+1) $ map length $ group n

trianglenum n = div (n * (n + 1)) 2

divnumtri n = product $ map (+1) $ map length $ group $ sort $ (delete 2) $ primeF (n) ++ primeF (n + 1)

divTriOver n = divTh n 2

divTh over n = if divnumtri(n) > over then div (n*(n+1)) 2 else divTh over (n+1)

testTri x = (mod' (sqrt(1 + 8*x) - 1) 1) == 0

--mindiv n =

--ratio list = map ratioh $ sort

--mindivh n list = if (product list) < n
--	then mindivh n (mini:list)
--	else list
--  where mini = minimum (ratio list)

--minDivB n = (minDiv n) $ (`take` primes) $ ((\(x,y) <- y) $ last $ takeWhile (< n*2) [(2^x, x) | x <- [1..]])

--minDiv n divi = if testTri (product divi)
--	then product divi
--	else minDiv n (change n (init divi))

--change n divi = if (numdiv' divi) > n
--		then divi
--		else (change n) $ (:divi) $ (primes !!) $ length $ takeWhile ()

primes = 2 : filter (null . tail . primeF) [3,5..]
primeF n = factor n primes
factor n (p:ps)
    | p*p > n        = [n]
    | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
    | otherwise      =     factor n ps

--listDiviseur n = sort $ listDivH $ group $ primeF n

sumDiviseur n = sum $ init $ listDivH $ primeF n
listDivH :: [[Int]] -> [Int]
listDivH (x:[]) = listMul(x)
listDivH (x:xs) = [x*y | x <- (listMul x), y <- (listDivH xs)]

listMul = scanl (*) 1



