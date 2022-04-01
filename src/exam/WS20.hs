module WS20 where
import Data.List (sort)

-- Assignment 1: Burrows-Wheeler-Transformation
indexOf :: String -> [String] -> Int
indexOf s (x:xs) = f s (x:xs) 0
    where
        f _ [] index = index
        f s (x:xs) index
            | s == x = index
            | otherwise = f s xs (index + 1)

rotations :: [a] -> [[a]]
rotations list = take n $ iterate rotateByOne list
    where n = length list

rotateByOne :: [a] -> [a]
rotateByOne list = list !! (n - 1) : take (n - 1) list
    where n = length list

transform :: String -> (Int, String)
transform s = (indexOf s sortedList, lastRow sortedList)
    where sortedList = sort $ rotations s

lastRow :: [String] -> String
lastRow = map (\ x -> x !! (length x - 1))

-- Assignment 2: Hashtrees
data Tree 
    = Node String Tree Tree
    | Leaf String Block

getHash :: Tree -> String 
getHash (Node h _ _) = h
getHash (Leaf h _) = h

mkLeaf :: Block -> Tree
mkLeaf block = Leaf (hashBlock block) block

fromBlocks :: [Block] -> Tree
fromBlocks blocks = foldr mkNode x xs 
    where (x:xs) = map mkLeaf blocks

check :: Tree -> Bool 
check (Node h t1 t2) = h == getHash (mkNode t1 t2) && check t1 && check t2
check (Leaf h b) = h == hashBlock b

condense :: [Tree] -> [Tree]
condense [] = []
condense [x] = [x]
condense (x:(y:ys)) = mkNode x y : condense ys

merge :: [Tree] -> Tree
merge [x] = x
merge xs = merge (condense xs)

fromBlocks2 :: [Block] -> Tree
fromBlocks2 blocks = merge (map mkLeaf blocks)