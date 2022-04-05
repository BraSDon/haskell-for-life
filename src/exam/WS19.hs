module WS19 where

substr :: Eq a => [a] -> [a] -> Bool
substr [] _ = True
substr _ [] = False
substr xs (y:ys) = prefix xs (y:ys) || substr xs ys

prefix :: Eq a => [a] -> [a] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) = x == y && prefix xs ys

data Queue a = Q [a] [a]

fromList :: [a] -> Queue a
fromList xs = Q xs []

toList :: Queue a -> [a]
toList (Q xs ys) = xs ++ reverse ys

enqueue :: a -> Queue a -> Queue a
enqueue x (Q front back) = Q front (x:back)

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue q
    | null $ toList q = Nothing
    | otherwise = f q
    where f (Q front back)
            | null front = Just (back !! (length back - 1), Q front (init back))
            | otherwise = Just (head front, Q (tail front) back)

data Tree t
    = Leaf
    | Node (Tree t) t (Tree t)

bfs :: Tree a -> [a]
bfs root = go $ fromList [root]

go :: Queue (Tree a) -> [a]
go q = addChild $ dequeue q
    where addChild (Just(Node t1 label t2, q')) = label : go (enqueue t2 $ enqueue t1 q')
          addChild _ = []

main :: IO()
main = do
    print $ substr "abcd" "abcd"
    print $ substr "abcd" "abcde"
    print $ substr "abcd" "abce"
    print $ substr "abcd" "ab"
    print . toList  $ enqueue 6 $ fromList [1,2,3,4,5]
    --print . f $ dequeue $ Q [] [1,2,3,4,5]
    --    where f (Just (x, q)) = (x, toList q)
    --          f Nothing = (0, [])
    
    print $ bfs $ Node (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) 4 (Node (Node Leaf 5 Leaf) 6 (Node Leaf 7 Leaf))
