module WS21 where

-- Assignment 1: Sprites
data Sprite =
    Sprite (Int -> Int -> Color)
        Int -- x_min 
        Int -- x_max
        Int -- y_min
        Int -- y_max

isActiveAt :: Int -> Int -> Sprite -> Bool
isActiveAt x y (Sprite f x_min x_max y_min y_max) = xInBounds && yInBounds
    where xInBounds x = x >= x_min && x <= x_max
          yInBounds y = y >= y_min && y <= y_max

translate :: Int -> Int -> Sprite -> Sprite
translate dx dy (Sprite f xmin xmax ymin ymax) = Sprite g (xmin + dx) (xmax + dx) (ymin + dy) (ymax + dy)
    where g x y = f (x - dx) (y - dy)

eval :: Sprite -> Int -> Int -> Color
eval (Sprite f _ _ _ _) = f

pixelAt :: Color -> [Sprite] -> Int -> Int -> Color
pixelAt background sprites x y
    | null activeSprites = background
    | otherwise = foldr (blend . (\s -> eval s x y)) background activeSprites
    where activeSprites = filter (isActiveAt x y) sprites

render :: Int -> Int -> Color -> [Sprite] -> [[Color]]
render width height background sprites = [[pixelAt background sprites x y| x <- [0..width - 1]] | y <- [0..height - 1]]

-- Assignment 2: Folds
dot :: [Double] -> [Double] -> Double
dot xs ys = sum $ zipWith (*) xs ys

tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = (x:xs) : tails xs

slide :: Int -> [a] -> [[a]]
slide n xs = [[xs !! current | current <- [lower..lower + n - 1]] | lower <- [0..]]

convolve :: [Double] -> [Double] -> [Double]
convolve mask signal = map (dot mask) (slide n signal) 
    where n = length mask

blur3 :: [Double] -> [Double]
blur3 = convolve [1/3.0, 1/3.0, 1/3.0]