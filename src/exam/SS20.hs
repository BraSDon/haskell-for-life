module SS20 where

-- Assignment 1: Socks
type Sock = Integer

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (y:ys)
    | x == y = ys
    | otherwise = y:delete x ys

setAt :: Int -> a -> [a] -> [a]
setAt i x list = setAtAcc i x list 0

setAtAcc :: Int -> a -> [a] -> Int -> [a]
setAtAcc _ _ [] _ = []
setAtAcc i x (y:ys) index
    | i == index = x:ys
    | otherwise = y : setAtAcc i x ys (index + 1)

maxStorage :: [Sock] -> Int
maxStorage socks = maxStorageAcc socks [] 0

maxStorageAcc :: [Sock] -> [Sock] -> Int -> Int
maxStorageAcc [] pile n = max n (length pile)
maxStorageAcc (x:xs) pile n
    | x `elem` pile = maxStorageAcc xs (delete x pile) n
    | otherwise = maxStorageAcc xs (x:pile) (max n (length pile + 1))

swap :: Int -> Int -> [a] -> [a]
swap i j list = setAt i t (setAt j (list !! i) list)
    where t = list !! j

shuffle :: [(Int, Int)] -> [a] -> [a]
shuffle [] list = list
shuffle ((i,j):xs) list = shuffle xs (swap i j list)

-- Assignment 2: Pattern Matching

data Tree
    = Leaf
    | Node Tree Tree

data Pattern
    = PLeaf
    | PNode Pattern Pattern
    | PVar String

type Subst = [(String, Tree)]

subst :: Subst -> Pattern -> Tree
subst sub (PVar s) = t
    where Just t = lookup s sub
subst sub PLeaf = Leaf
subst sub (PNode p1 p2) = Node (subst sub p1) (subst sub p2)

liftMaybe2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftMaybe2 f (Just a) (Just b) = Just (f a b)
liftMaybe2 f _ _ = Nothing 

match :: Pattern -> Tree -> Maybe Subst
match (PNode p1 p2) (Node n1 n2) = liftMaybe2 (++) (match p1 n1) (match p2 n2)
match (PVar v) t = Just [(v,t)]
match PLeaf Leaf = Just []
match _ _ = Nothing 

matchFirst :: [(Pattern, Subst -> a)] -> Tree -> a
matchFirst ((pat, f):alts) tree = g (match pat tree)
    where g (Just sub) = f sub
          g _ = matchFirst alts tree

main :: IO()
main = do
    print $ maxStorage [1,2,3,1,2,3]
    print $ maxStorage [1,1,2,2,3,3,4,4,4]
    print $ swap 1 2 [0,1,2,3]