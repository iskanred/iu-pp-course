module Main (main) where

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.Char (toUpper)

-----------------------------------------------------------------------------
------------------------ Iskander Nafikov BS20-SD-01 ------------------------
----------------------- i.nafikov@innopolis.university ----------------------
-----------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  _ <- sequenceMaybeIO [maybeIO (Just (putStrLn "hello")), maybeIO Nothing, maybeIO (Just (putStrLn "ell")), maybeIO Nothing]
  print ""


------------- Exercise 1 -------------

guess :: (a -> Bool) -> (String -> IO a) -> IO a
guess p g = do
  s <- getLine
  x <- g s
  case p x of
    True -> return x
    False -> guess p g

-- 'p' argument is some predicate that accepts some argument of type (a) and returns (Bool)
-- 'g' argument is function of string that returns (IO a),
--     we can obtain it from "g s" where 's' is (String) and "x <- g s" which means that 'g' returns (IO a)
-- returning type is (IO a) because of "return x" function call where 'x' is (a)


------------- Exercise 2 -------------

echo :: IO ()
echo = do
  putStr "Enter a string: "
  input <- getLine
  putStrLn ("Result: " ++ map toUpper input)
  echo


------------- Exercise 3 -------------

---- a)
foreverIO :: IO a -> IO b
foreverIO program = do
  _ <- program
  foreverIO program

---- b)
whenIO :: Bool -> IO() -> IO()
whenIO cond program =
  case cond of
    True -> program
    False -> return ()

---- c)
maybeIO :: Maybe (IO a) -> IO (Maybe a)
maybeIO Nothing = return Nothing
maybeIO (Just program) = do
  programContent <- program
  return (Just programContent)
  
---- d)
sequenceMaybeIO :: [IO (Maybe a)] -> IO [a]
sequenceMaybeIO [] = return []
sequenceMaybeIO (program : programs) = do
  programContent <- program
  case programContent of
    Nothing -> sequenceMaybeIO programs
    Just programContentNotEmpty -> do
      programsContent <- sequenceMaybeIO programs
      return (programContentNotEmpty:programsContent)

---- e)
whileJustIO :: (a -> IO (Maybe a)) -> a -> IO ()
whileJustIO func val = do
  programContent <- func val
  case programContent of
    Nothing -> return ()
    Just programContentNotEmpty -> whileJustIO func programContentNotEmpty

---- f)
forStateIO_ :: s -> [a] -> (a -> s -> IO s) -> IO s
forStateIO_ val [] _ = return val
forStateIO_ val (current : rest) func = do
  nextVal <- func current val
  forStateIO_ nextVal rest func

verboseCons :: Int -> [Int] -> IO [Int]
verboseCons x xs = do
  putStrLn ("prepending " ++ show x ++ " to " ++ show xs)
  return (x:xs)


------------- Exercise 4 -------------

iforIO_ :: [a] -> (Int -> a -> IO ()) -> IO ()
iforIO_ values func = helper 0 values
  where
    helper _ [] = return ()
    helper index (val : rest) = do
      func index val
      helper (index + 1) rest

example = do
  iforIO_ [1, 2] (\i n ->
    iforIO_ "ab" (\j c ->
      print ((i, j), replicate n c)))
