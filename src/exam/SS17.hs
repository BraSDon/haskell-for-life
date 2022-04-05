module SS17 where

magicNumber :: Int -> Int
magicNumber n = div (sum [i | i <- [1..n*n]]) n

duplicates :: Eq a => [a] -> Bool
duplicates [] = False
duplicates (x:xs) = x `elem` xs || duplicates xs

transpose :: [[a]] -> [[a]]
transpose xs = [[element xs i j | i <- [0..length xs - 1]] | j <- [0..length (head xs) - 1]]
    where element xs i j = xs !! i !! j

isMagic :: [[Int]] -> Bool
isMagic xs = not (duplicates $ concat xs) && all (\x -> sum x == magicNumber n) (xs ++ transpose xs) 
    where n = length $ head xs

data Tree = Node [Tree] | Leaf State
data State = Dead | Alive

prune :: Tree -> Tree
prune (Leaf l) = Leaf l
prune (Node ts) = Node $ filter isAlive $ map prune ts
    where isAlive (Node []) = False 
          isAlive (Leaf Dead) = False
          isAlive _ = True 

main :: IO()
main = do
    print $ transpose [[1,2,3],[4,5,6],[7,8,9]]
    print $ isMagic [[1,2,3],[4,5,6],[7,8,9]]
    print $ isMagic [[6,2,7], [1,9,5], [8,4,3]]
