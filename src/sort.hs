module Sort where


insert :: Ord t => [t] -> t -> [t]
insert [] a = [a]
insert (x : xs) a
    | a < x = a : (x : xs)
    | otherwise = x : insert xs a

insertSort :: Ord t => [t] -> [t] -> [t]
insertSort xs accu = foldl insert accu xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge ys [] = ys
merge (x : xs) (y : ys)
    | x <= y = x : merge xs (y : ys)
    | otherwise = y : merge (x : xs) ys

mergeSort :: Ord t => [t] -> [t]
mergeSort [x] = [x]
mergeSort [] = []
mergeSort xs = merge (mergeSort front) (mergeSort back)
    where (front, back) = splitAt (length xs `div` 2) xs

main :: IO()
main = do
    print(insertSort [1,5,2,3] [])
    print(mergeSort [1,5,2,3])