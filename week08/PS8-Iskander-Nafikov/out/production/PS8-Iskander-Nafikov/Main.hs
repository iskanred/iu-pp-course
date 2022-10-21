module Main (main) where


-----------------------------------------------------------------------------
------------------------ Iskander Nafikov BS20-SD-01 ------------------------
----------------------- i.nafikov@innopolis.university ----------------------
-----------------------------------------------------------------------------


main :: IO ()
main = print ""

notP :: (a -> Bool) -> (a -> Bool)
notP = (not .)


------------- Exercise 1 -------------

---- a)
isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _ = False

---- b)
insert :: Int -> [Int] -> [Int]
insert numToInsert [] = [numToInsert]
insert numToInsert xs = takeWhile (numToInsert >) xs ++ [numToInsert] ++ dropWhile (numToInsert >) xs

---- c)
separateBy :: a -> [a] -> [a]
separateBy _ [x] = [x]
separateBy delim (x : xs) = [x, delim] ++ separateBy delim xs

---- d)
splitWhenNot :: (a -> Bool) -> [a] -> ([a], [a])
splitWhenNot _ [] = ([], [])
splitWhenNot predicate xs = (takeWhile predicate xs, dropWhile predicate xs)

---- e)
groupsSeparatedBy :: (a -> Bool) -> [a] -> [[a]]
groupsSeparatedBy _ [] = []
groupsSeparatedBy predicate xs =
  takeWhile (notP predicate) (dropWhile predicate xs)
  :
  groupsSeparatedBy predicate (dropWhile predicate (dropWhile (notP predicate) (dropWhile predicate xs)))

---- f)
replicateWithPos :: [a] -> [a]
replicateWithPos = helper 1
  where
    helper :: Int -> [a] -> [a]
    helper _ [] = []
    helper i (x : rest) = replicate i x ++ helper (i + 1) rest


------------- Exercise 2 -------------

---- a)
lucas :: [Int]
lucas = 2 : helper 2 1
  where
    helper :: Int -> Int -> [Int]
    helper x1 x2 = x2 : helper x2 (x1 + x2)

---- b)
approximationsOfRoot2 :: Double -> [Double]
approximationsOfRoot2 x = next : approximationsOfRoot2 next
  where next = x - x / 2 + 1 / x