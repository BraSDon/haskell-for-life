module Combinators where

type Polynome = [Double]

cmult :: Num b => [b] -> b -> [b]
cmult p c = map (*c) p

eval :: (Foldable t, Num a) => t a -> a -> a
eval p x = foldr (\a b -> a + x * b) 0 p

deriv :: (Num c, Enum c) => [c] -> [c]
deriv p = zipWith (*) [1..] (tail p)

main :: IO()
main = do
    print()