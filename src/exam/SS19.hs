module SS19 where

-- Assignment 1: Sorted Listsums
splay :: [a] -> [[a]]
splay = map (\ x -> [x | i <- [0 .. ]])

zziipp :: Num a => [[a]] -> [[a]] -> [[a]]
zziipp xs ys = zipWith (+) a b : zziipp as bs
    where n = min (length xs) (length ys)
          (a:as) = take n xs
          (b:bs) = take n ys

distrib :: Num a => [a] -> [a] -> [[a]]
distrib xs [] = [xs]
distrib [] ys = [ys]
distrib (x:xs) ys = map (+ x) ys : distrib xs ys

mergeAll :: Ord a => [[a]] -> [a]
mergeAll [] = []
mergeAll [x] = x
mergeAll (x:y:ys)
    | head x <= head y = head x : mergeAll (tail x:y:ys)
    | otherwise = head y : mergeAll (merge x y:ys)

xadd :: (Ord a, Num a) => [a] -> [a] -> [a]
xadd xs ys = mergeAll $ distrib xs ys

-- Assignment 2: Higher order functions
insert :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
insert k v = alter f k
    where f (Just val) = Just v
          f Nothing = Just v

update :: Eq k => (v -> v) -> k -> [(k,v)] -> [(k,v)]
update f = alter g
    where g (Just val) = Just (f val)
          g Nothing = Nothing

alter :: Eq k => (Maybe v -> Maybe v) -> k -> [(k,v)] -> [(k,v)]
alter f k [] = help k $ f Nothing
alter f k ((k', v'):m)
    | k' == k = help k (f (Just v')) ++ m
    | otherwise = (k', v') : alter f k m

help :: a -> Maybe b -> [(a, b)]
help k (Just v) = [(k,v)]
help _ Nothing = []