module Streams where

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

collatz :: Integer -> [Integer]
collatz = iterate f
    where f x = if even x then x `div` 2 else 3 * x + 1

num :: Integer -> Integer
num m = firstOccurence 1 (collatz m) 0

firstOccurence :: (Eq t1, Num t2) => t1 -> [t1] -> t2 -> t2
firstOccurence x [] i = error "element not in list"
firstOccurence x (y:ys) i
    | x == y = i
    | otherwise = firstOccurence x ys (i+1)

maxNum :: Integer -> Integer -> (Integer, Integer)
maxNum a b = maxNumAcc a b (0,0)

maxNumAcc :: Integer -> Integer -> (Integer, Integer) -> (Integer, Integer)
maxNumAcc a b acc
    | a > b = acc
    | current > snd acc = maxNumAcc (a+1) b (a, current)
    | otherwise = maxNumAcc (a+1) b acc
        where current = num a

-- Merge and Prime
merge :: [Integer] -> [Integer] -> [Integer]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

mergeAll :: [[Integer]] -> [Integer]
mergeAll [] = []
mergeAll [x] = x
mergeAll (x:xs) = head x : mergeAll (insertion (tail x) xs)

insertion :: Ord a => [a] -> [[a]] -> [[a]]
insertion x [] = [x]
insertion [] y = y
insertion x (y:ys)
    | head x < head y = x:(y:ys)
    | otherwise = y : insertion x ys

allprimepowers :: [Integer]
allprimepowers = mergeAll [map (^i) primes | i <- [1..]]

primepowers :: Integer -> [Integer]
primepowers n = foldr merge [] [map (^i) primes | i <- [1..n]]

odds :: [Integer]
odds = 1 : map (+2) odds

oddPrimes :: [Integer] -> [Integer]
oddPrimes (p:ps) = p : oddPrimes [px | px <- ps, (mod px p) /= 0]

primes :: [Integer]
primes = 2 : oddPrimes (tail odds)



main :: IO ()
main = do
    print $ insertion [2, 3] [[1,4], [3,4]]
    print $ mergeAll [[1,2,3], [2,4,8], [3,9,27]]
    print $ take 10 allprimepowers