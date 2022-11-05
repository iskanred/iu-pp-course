module Main (main) where

import System.Random (StdGen, newStdGen, randomRs)

randomDigits :: StdGen -> [Int]
randomDigits g = randomRs (0, 9) g

main :: IO ()
main = putStrLn ""


-- ex1
implies :: Bool -> Bool -> Bool
implies False _ = True
implies True bool = bool


-- ex2
divides :: Int -> Int -> Bool
divides x y = y `mod` x == 0

imply :: [Bool] -> Bool -> Bool
imply xs y = and xs `implies` y
    

-- ex3
cond :: [(Bool, a)] -> a -> a
cond [] dflt = dflt
cond ((False, _) : rest) dflt = cond rest dflt
cond ((_, expr) : _) _ = expr


-- ex4
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf size xs = take size xs : chunksOf size (drop size xs)


-- ex5
af = do
  Just return