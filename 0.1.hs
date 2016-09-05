import Data.List as L

--trick on number
reversal :: Integral a => a -> a
reversal = go 0
  where go a 0 = a
        go a b = let (q,r) = b `quotRem` 10 in go (a*10 + r) q


midList list = (list !! (leng -1), list !! leng)
    where leng = div (length list) 2

digs 0 = []
digs x = digs (x `div`10) ++ [mod x 10]

btw mini maxi num = (num >= 100) && (num <= 999)
btw2 (x,y) mini maxi = (btw mini maxi x) && (btw mini maxi y)

windows n list = take ((length list2) - n) list2
    where list2 = windows' (n - 1) list
windows' n [] = [[]]
windows' n (x:xs) =  (x:(take n xs)):(windows' n xs)

--collatz sequence

collatz 1 = 1
collatz x = if even x then (collatz (div x 2)) + 1 else (collatz (3*x + 1)) + 1

--palindrome number

listPalindromeN 1 = [1..9]
listPalindromeN n = if even n
        then [10^divn*x + (reversal x)| x <- [10^(divn)-1,10^(divn)-2..10^(divn - 1)]]
        else [10^(divn+1)*x+(reversal x)+y*10^divn| x <- [10^(divn)-1,10^(divn)-2..10^(divn - 1)], y <- [9,8..1]]
    where divn = div n 2


palindrome = ([x | x <- listPalindromeN 6, btw2 (midList(listDiv x)) 100 999]) !! 0
--1000number

-- faster nub
nub' x = map (head) $ L.group $ sort x

-- sum of arithmetic list
atmcListSum mini maxi step = div ((maxi + mini) * num) 2 where num = 1 + div (maxi - mini) step

-- get non-abundant sum, not working yet
perfectAndDeficientNumber = takeWhile (<(div 28123 2)) [x | (x, y) <- sumDivNumber, x >= y]
nonAbundantSum = atmcListSum 0 28123 1 - (sum $ nub' $ [ x+y | x <- perfectAndDeficientNumber, y <- perfectAndDeficientNumber])

--perfect/abundant/deficient number
sumDivNumber = [(x, sumDiv x) | x <- [1..]]
abundantNumber = [x | (x, y) <- sumDivNumber, x < y]
perfectNumber = [x | (x, y) <- sumDivNumber, x == y]
deficientNumber = [x | (x, y) <- sumDivNumber, x > y]

--divisor stuff
sumDiv = sum . init . listDiv

--listDivInf = [(x,y)|
listDiv = sort . listDivH . L.group . primeF
listDivH :: [[Integer]] -> [Integer]
listDivH (x:[]) = listMul(x)
listDivH (x:xs) = [x*y | x <- (listMul x), y <- (listDivH xs)]

listMul = scanl (*) 1
--

divnumtri n = product $ map (+1) $ map length $ group $ sort $ delete 2 $ primeF (n) ++ primeF (n + 1)

divTriOver n = divTh n 2
divTh over n = if divnumtri(n) > over then div (n*(n+1)) 2 else divTh over (n+1)

--triplet pythagorian

tripletPythagore = [(x,y,z)| x <- [1..], y <- [1..x], z <- [1..y], (x+y+z) == 1000, x^2 == (y^2 + z^2)]

--prime number stuff
primes = 2 : filter (null . tail . primeF) [3,5..]

primeF n = factor n primes

factor n (p:ps)
    | p*p > n        = [n]
    | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
    | otherwise      =     factor n ps

primeUntil n = (2:) $ primeUntilH (truncate(sqrt(fromInteger n :: Float))) [3,5..n]
primeUntilH :: Integer -> [Integer] -> [Integer]
primeUntilH n (x:xs) = if x > n
        then (x:xs)
        else primeUntilH n (x:([y | y <- xs, (mod y x) /= 0]))

numT = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450
