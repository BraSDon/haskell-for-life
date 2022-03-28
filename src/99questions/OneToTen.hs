module OneToTen where

-- Problem 1: Find last element of a list
lastElement :: [a] -> a
lastElement l
    | null l = error "List does not contain atleast 1 element"
    | otherwise = l !! (length l - 1)

-- Problem 2: Find second to last element of a list
secondLastElement :: [a] -> a
secondLastElement l
    | length l <= 1 = error "List does not contain atleast 2 elements"
    | otherwise = l !! (length l - 2)

-- Problem 3: Find the kTh element (zero indexed)
kthElement :: [a] -> Int -> a
kthElement l k
    | length l < k = error "List does not contain at least k elements"
    | otherwise = l !! k

-- Problem 4: Find number of elements in list
len :: [a] -> Int
len l = sum $ map (const 1) l

-- Problem 5: Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6: Check if list is palindrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome l
    | even (length l) = listEqual (myReverse frontHalf) backHalf
    | otherwise = listEqual (myReverse frontHalf) (tail backHalf)
    where frontHalf = take (div (length l) 2) l
          backHalf = drop (div (length l) 2) l

listEqual :: Eq a => [a] -> [a] -> Bool
listEqual [] [] = True
listEqual x [] = False
listEqual [] y = False
listEqual (x:xs) (y:ys)
    | x == y = listEqual xs ys
    | otherwise = False

-- Problem 7: Flatten a nested list structure (concat)
flatten :: [[a]] -> [a]
flatten = foldr (++) []

-- Problem 8: Eliminate consecutive duplicates of list elements
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:(y:ys))
    | x == y = compress (y:ys)
    | otherwise = x : compress (y:ys)

-- Problem 9: Pack consecutive duplicates of list elements into sublists
pack :: Eq a => [a] -> [[a]]

main :: IO()
main = do
    print $ len [1,2,3,4,5]
    print $ myReverse [1,2,3,4,5]
    print $ isPalindrome [1,2,3,3,2,1]
    print $ isPalindrome [1,2,3,2,1]
    print $ flatten [[1,2],[5,8],[8,2,3,5,6]]
    print $ compress [1,1,2,2,3,3,4,4,4,5,5]