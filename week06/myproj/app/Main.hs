--------------------------- Problem Set #6 ---------------------------
--------------------- Iskander Nafikov B20-SD-01 ---------------------
------------------- i.nafikov@innopolis.university -------------------

module Main (main) where

main :: IO ()
main = print ()


type Name = String
data Grade = A | B | C | D
data Student = Student Name Grade
data Result a = Success a | Failure String

dup f x = f x x
dip f x = f (f x x)
twice f x = f (f x)


--------------------- Exercise 1 ---------------------

-- First, we present the polymorphic types:
dup :: (a -> a -> b) -> a -> b
dip :: (a -> a -> a) -> a -> (a -> a)
twice :: (a -> a) -> a -> a

---- a) dip (+) 1 2
-- *  dip :: (a1 -> a1 -> a1) -> a1 -> (a1 -> a1)
-- *  (+) :: Int -> Int -> Int
-- *  1 :: Int
-- *  2 :: Int
--
-- Int -> Int -> Int = (a1 -> a1 -> a1) [from 1st argument]
-- a1 = Int  [from 2nd argument and equation above]
--
-- So:
-- dip (+) 1 :: (Int -> Int)
--
-- Therefore:
-- dip (+) 1 2 :: Int


---- b) dup (dip (+)) 1
-- *  dup :: (a1 -> a1 -> b1) -> a1 -> b1
-- *  dip :: (a2 -> a2 -> a2) -> a2 -> (a2 -> a2)
-- *  (+) :: Int -> Int -> Int
-- *  1 :: Int
--
-- Int -> Int -> Int = (a2 -> a2 -> a2) [from 1st argument of dip]
-- a2 = Int
--
-- So:
-- dip (+) :: Int -> (Int -> Int)
--
-- Int -> (Int -> Int) = Int -> Int -> Int = (a1 -> a1 -> b1) [from 1st argument of dup]
-- a1 = Int, b1 = Int
--
-- So:
-- dup (dip (+)) :: Int -> Int
--
-- Therefore:
-- dup (dip (+)) 1 :: Int


---- c) twice dip
-- *  twice :: (a1 -> a1) -> a1 -> a1
-- *  dip :: (a2 -> a2 -> a2) -> a2 -> (a2 -> a2)
--
-- (a2 -> a2 -> a2) -> a2 -> (a2 -> a2) = (a1 -> a1)
-- (a2 -> a2 -> a2) -> (a2 -> a2 -> a2) = (a1 -> a1)
-- a1 = (a2 -> a2 -> a2)
--
-- Therefore:
-- twice dip :: (a2 -> a2 -> a2) -> (a2 -> a2 -> a2)


---- d) dip dip
-- *  dip1 :: (a1 -> a1 -> a1) -> a1 -> (a1 -> a1)
-- *  dip2 :: (a2 -> a2 -> a2) -> a2 -> (a2 -> a2)
--
-- (a2 -> a2 -> a2) -> a2 -> (a2 -> a2) = (a1 -> a1 -> a1)
-- ((a2 -> a2 -> a2) -> (a2 -> (a2 -> a2))) = (a1 -> (a1 -> a1))
-- a1 = (a2 -> a2 -> a2), a1 = a2, a1 = (a2 -> a2)
-- a2 = (a2 -> a2)
-- We see that a2 is infinite type, so expression leads to TYPE ERROR


---- e) twice twice twice
-- *  twice1 :: (a1 -> a1) -> a1 -> a1
-- *  twice2 :: (a2 -> a2) -> a2 -> a2
-- *  twice3 :: (a3 -> a3) -> a3 -> a3
--
-- (a3 -> a3) -> a3 -> a3 = (a2 -> a2)
-- ((a3 -> a3) -> (a3 -> a3)) = (a2 -> a2)
-- a2 = a3 -> a3
-- 
-- (a2 -> a2) -> a2 -> a2 = (a1 -> a1)
-- a1 = a2 -> a2 = (a3 -> a3) -> a3 -> a3
-- 
-- Therefore:
-- twice twice twice :: (a3 -> a3) -> a3 -> a3


---- f) dup twice
-- *  dup :: (a1 -> a1 -> b1) -> a1 -> b1
-- *  twice :: (a2 -> a2) -> a2 -> a2
--
-- (a2 -> a2) -> a2 -> a2 = (a1 -> a1 -> b1)
-- a1 = (a2 -> a2), a1 = a2, a2 = b1
-- a2 = (a2 -> a2)
-- We see that a2 is infinite type, so expression leads to TYPE ERROR



--------------------- Exercise 2 ---------------------

studentsWithA :: [Student] -> [Name]
studentsWithA [] = []
studentsWithA(Student name A : rest) = name : studentsWithA rest
studentsWithA(_:rest) = studentsWithA rest



--------------------- Exercise 2 ---------------------

---- a)
whileSuccess :: (a -> Result a) -> a -> a
whileSuccess func val = helper (func val) val
  where
    helper (Failure _) prev = prev
    helper (Success val) _ = helper (func val) val


---- b)
applyResult :: Result (a -> b) -> Result a -> Result b
applyResult (Success func) (Success arg) = Success $func arg
applyResult (Success _) (Failure errMsg) = Failure errMsg
applyResult (Failure errMsg) _ = Failure errMsg


---- c)
fromResult :: (a -> b) -> (String -> b) -> Result a -> b
fromResult actionIfSuccess _ (Success val) = actionIfSuccess val
fromResult _ actionIfFailure (Failure errMsg) = actionIfFailure errMsg


---- d)
combineResultsWith :: (a -> b -> c) -> Result a -> Result b -> Result c
combineResultsWith func (Success val1) (Success val2) = Success $func val1 val2
combineResultsWith _ (Success _) (Failure errMsg) = Failure errMsg
combineResultsWith _ (Failure errMsg) _ = Failure errMsg


a = do
      x <- return getLine
      return x
