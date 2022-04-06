module WS18 where

sumDQ :: [Float] -> Float
sumDQ [] = 0
sumDQ [x] = x
sumDQ xs = sumDQ (take n xs) + sumDQ (drop n xs)
    where n = div (length xs) 2

data Rope
    = Leaf String
    | Inner Rope Int Rope

ropeLength :: Rope -> Int
ropeLength (Leaf s) = length s
ropeLength (Inner _ n r) = n + ropeLength r

ropeConcat :: Rope -> Rope -> Rope
ropeConcat r1 r2 = Inner r1 n r2
    where n = ropeLength r1

ropeSplitAt :: Int -> Rope -> (Rope, Rope)
ropeSplitAt i r
    | i < 0 || i > ropeLength r = error"out of bounds"
    | otherwise = fromString (toString r) i

fromString :: [Char] -> Int -> (Rope, Rope)
fromString xs i = (Leaf l, Leaf r)
    where (l, r) = splitAt i xs

toString :: Rope -> [Char]
toString (Leaf s) = s
toString (Inner r1 _ r2) = toString r1 ++ toString r2

main :: IO ()
main = do
    print $ sumDQ [1..100]
    let (l,r) = ropeSplitAt 4 (ropeConcat (Inner (Leaf "abc") 3 (Leaf "def")) (Leaf "ghi"))
    print (toString l, toString r)