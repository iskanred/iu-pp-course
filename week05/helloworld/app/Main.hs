module Main (main) where
  
main :: IO ()
main = print (alternatingSum [1,2,3,4,5])

--------------------------- Utility functions ---------------------------

cutLeadingZeros :: [Int] -> [Int]
cutLeadingZeros [] = [0]
cutLeadingZeros (0:rest) = cutLeadingZeros rest
cutLeadingZeros binary = binary


--------------------------- Exercise 1 ---------------------------

-- a)
binaryToDecimal :: [Int] -> Int
binaryToDecimal binary = helper (reverse binary) 0 1
  where
    helper :: [Int] -> Int -> Int -> Int
    helper (bit:rest) result pow = helper rest (result + bit * pow) (pow * 2)
    helper [] result _ = result

-- b)
countZeros :: [Int] -> Int
countZeros binary = helper (cutLeadingZeros binary) 0
  where
    helper :: [Int] -> Int -> Int
    helper [] result = result
    helper (0:rest) result = helper rest (result + 1)
    helper (1:rest) result = helper rest result

-- c)
encodeWithLengths :: [Int] -> [Int]
encodeWithLengths binary = helper (cutLeadingZeros binary) 1 0 []
  where
    helper :: [Int] -> Int -> Int -> [Int] -> [Int]
    helper [0] _ _ _ = [1]  -- starting bit is 1 so we need mark this case explicitly
    helper [] _ acc result = result ++ [acc]
    helper (0:rest) 0 acc result = helper rest 0 (acc + 1) result      -- first = 0, currentBit = 0
    helper (0:rest) 1 acc result = helper rest 0 1 (result ++ [acc])   -- first = 0, currentBit = 1
    helper (1:rest) 0 acc result = helper rest 1 1 (result ++ [acc])   -- first = 1, currentBit = 0
    helper (1:rest) 1 acc result = helper rest 1 (acc + 1) result      -- first = 1, currentBit = 1

-- d)
binaryOdd :: [Int] -> Bool
binaryOdd binary = helper binary 0
  where
    helper :: [Int] -> Int -> Bool
    helper [] lastBit = lastBit == 1
    helper (bit:rest) _ = helper rest bit


-- e)
decrement :: [Int] -> [Int]
decrement binary = cutLeadingZeros (helper (reverse (cutLeadingZeros binary)) [] True)
  where
    helper :: [Int] -> [Int] -> Bool -> [Int]
    helper [0] _ _ = [0]
    helper [] result _ = result
    helper (bit:rest) result False = helper rest (bit : result) False
    helper (1:rest) result True = helper rest (0 : result) False
    helper (0:rest) result True = helper rest (1 : result) True

-- d)
propagate :: (Bool, [Int]) -> [(Bool, Int)]
propagate (boolVal, ints) = helper boolVal ints [] 
  where
    helper :: Bool -> [Int] -> [(Bool, Int)] -> [(Bool, Int)]
    helper _ [] result = result
    helper boolVal (int:rest) result = helper boolVal rest (result ++ [(boolVal, int)])
 
 
--------------------------- Exercise 2 ---------------------------

alternatingSum :: [Int] -> Int
alternatingSum ints = helper ints 0 True
  where
    helper :: [Int] -> Int -> Bool -> Int
    helper [] result _ = result
    helper (int:rest) result True = helper rest (result + int) False
    helper (int:rest) result False = helper rest (result - int) True

    
---- Equational reasoning ----
-- alternatingSum [1,2,3,4,5]
-- = helper [1,2,3,4,5] 0 True
-- = helper (1:[2,3,4,5]) 0 True
-- = helper (2:[3,4,5]) 1 False 
-- = helper (3:[4,5]) -1 True
-- = helper (4:[5]) 2 False
-- = helper (5:[]) -2 True
-- = helper [] 3 True
-- = 3

--------------------------- Exercise 3 ---------------------------

data Radians = Radians Double
data Degrees = Degrees Double

pi :: Double
pi = 3.14159

toDegrees :: Radians -> Degrees
toDegrees (Radians rad) = Degrees (rad * (180.0 / Main.pi))

fromDegrees :: Degrees -> Radians
fromDegrees (Degrees deg) = Radians (deg * (Main.pi / 180))


---- for printing purposes

radiansToDouble :: Radians -> Double
radiansToDouble (Radians rad) = rad

degreesToDouble :: Degrees -> Double
degreesToDouble (Degrees deg) = deg
