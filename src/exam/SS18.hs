module SS18 where

properDivisors :: Integer  -> [Integer]
properDivisors n = f n 1
    where f n i
            | i > n = []
            | mod n i == 0 = i : f n (i + 1)
            | otherwise = f n (i + 1)

perfectNumber :: Integer -> Bool
perfectNumber n = sum (properDivisors n) == n

properDivisorsOpt :: Integer -> [Integer]
properDivisorsOpt n = 1 : f n 2
    where f n i
            | 2 <= i && i * i < n && mod n i == 0 = i : div n i : f n (i + 1)
            | i * i == n = i : f n (i + 1)
            | otherwise = []

data ListBuilder a
    = Nil
    | Cons a (ListBuilder a)
    | Append (ListBuilder a) (ListBuilder a)

fromList :: [a] -> ListBuilder a
fromList = foldr Cons Nil

rev :: ListBuilder a -> ListBuilder a
rev (Append l1 l2) = Append (rev l2) (rev l1)
rev (Cons x l) = Append (rev l) (Cons x Nil)
rev Nil = Nil

toListAcc :: ListBuilder a -> [a] -> [a]
toListAcc (Append l1 l2) acc = toListAcc l2 (toListAcc l1 acc)
toListAcc (Cons x l) acc = toListAcc l (acc ++ [x])
toListAcc Nil acc = acc

toList :: ListBuilder a -> [a]
toList lb = toListAcc lb []

main :: IO ()
main = do
    print $ toList $ fromList [1..10]
    print $ toList $ Append (Append (Append (fromList [1..10]) (fromList [11..20])) (fromList [21..30])) (fromList [31..40])