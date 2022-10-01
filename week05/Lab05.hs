import CodeWorld

bit0 :: Picture
bit0 = rectangle 1 1

bit1 :: Picture
bit1 = solidRectangle 1 1

renderBit :: Int -> Picture
renderBit bitValue
  | bitValue == 1 = bit1
  | bitValue == 0 = bit0
  | otherwise = error "illegal value of a bit"

renderBits :: [Int] -> Picture
renderBits (bitValue : bits) =
  renderBit bitValue <> translated 1.05 0
                          (renderBits bits)                       
renderBits [] = blank


countOnes :: [Int] -> Int
countOnes (1:bits) = 1 + countOnes bits
countOnes (0:bits) = countOnes bits
countOnes (x:bits) = error "illegal value of a bit"
countOnes [] = 0


trailingZeros :: [Int] -> Int
trailingZeros bits = helper bits 0
  where
    helper :: [Int] -> Int -> Int
    helper (1:restBits) acc = helper restBits 0
    helper (0:restBits) acc = helper restBits (acc + 1)
    helper [] acc = acc

main :: IO()
main = print (trailingZeros [1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0])
