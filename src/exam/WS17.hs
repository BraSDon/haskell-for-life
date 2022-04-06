module WS17 where

errChar :: Char
errChar = ' '

data Bit = Zero | One

instance Show Bit where
  show Zero = "0"
  show One  = "1"

data HuffmanTree
    = Node HuffmanTree HuffmanTree
    | Leaf Char

decode :: HuffmanTree -> [Bit] -> [Char]
decode _ [] = []
decode t xs
    | c /= errChar = c : decode t rest
    | otherwise = []
    where (c, rest) = decodeSeq t xs

decodeSeq :: HuffmanTree -> [Bit] -> (Char, [Bit])
decodeSeq (Leaf c) xs = (c, xs)
decodeSeq (Node _ t2) (One:xs) = decodeSeq t2 xs
decodeSeq (Node t1 _) (Zero:xs) = decodeSeq t1 xs
decodeSeq _ [] = (errChar, [])

encode :: HuffmanTree -> [Char] -> [Bit]
encode t = concatMap (encodeOne t)

encodeOne :: HuffmanTree -> Char -> [Bit]
encodeOne t c = snd $ head $ filter (\x -> fst x == c) (getEncodings t [])

getEncodings :: HuffmanTree -> [Bit] -> [(Char, [Bit])]
getEncodings (Leaf c) bits = [(c, bits)]
getEncodings (Node t1 t2) xs = getEncodings t1 (xs++[Zero]) ++ getEncodings t2 (xs++[One])

main :: IO ()
main = do
    print $ getEncodings (Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')) []
    let t = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')
    print $ encode t "cabc"
    print $ decode t (encode t "cabc")