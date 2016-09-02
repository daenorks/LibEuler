r = 5

xor arg1 arg2 = arg1 && not arg2 || arg2 && not arg1

perimetreCercle r = 2*pi*r

sign r = if r > 0 then "positif" else "negatif"

myMin a b = if a < b then a else b

myMax a b = if b > a then b else a

myin a b c d = myMin (myMin a b) (myMin c d)

bornerDans born1 born2 val = if born1 < born2
		then bornerDans2 born1 born2 val
		else bornerDans2 born2 born1 val

bornerDans2 min max val = if val < min
		then min
		else myMin max val

inBorn min max val = myMin max (myMax min val) == val
sommeVec (x, y) (x2, y2) = (x + x2, y + y2)

paire = (12, 'a')

triplet = (42,23,paire)


--fibonacciSum = 

--fibonacci = 

norme (x, y) = (x^2 + y^2) ** 0.5
sum2Multiple :: Integer -> Integer -> Integer -> Integer
sum2Multiple mul1 mul2 number = (sumMultiple mul1 number) + (sumMultiple mul2 number) - (sumMultiple (mul1 * mul2) number)

sumMultiple:: Integer -> Integer -> Integer
sumMultiple mul number = div (ntime * (ntime + 1) * mul) 2
	where ntime = div (number - 1) mul

primeNumBeg number = primeNum number 2 []
primeNum:: Int -> Int -> [Int] -> [Int]
primeNum 1 prime list = list
primeNum number prime list = if (mod number prime) == 0 then primeNum (div number prime) prime (prime:list) else primeNum number (prime + 1) list

