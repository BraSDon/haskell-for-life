module Test where

tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = (x:xs) : tails xs

slide :: Int -> [a] -> [[a]]
slide n xs = [[xs !! current | current <- [lower..lower + n - 1]] | lower <- [0..]]

dot :: [Double] -> [Double] -> Double
dot xs ys = sum $ zipWith (*) xs ys

convolve :: [Double] -> [Double] -> [Double]
convolve mask signal = map (dot mask) (slide n signal) 
    where n = length mask

main :: IO()
main = do
    print $ tails [1,2,3,4,5]
    print $ take 10 $ slide 3 [1,2..]
    print $ take 10 $ convolve [1, 0.1] [1,2..]
    