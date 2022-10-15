module Main (main) where
  
import Text.Read (readMaybe)
import System.IO(hSetBuffering, stdout, BufferMode(..))

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  sumOfTwoInputs2

sumOfTwoInputs :: IO()
sumOfTwoInputs = do
  str1 <- getLine
  str2 <- getLine
  case readMaybe str1 :: Maybe Int of
    Nothing -> do
      putStrLn "The first number is invlaid!"
    Just n1 -> do
      case readMaybe str2 of
        Nothing -> do
          putStrLn "The second number is invalid!"
        Just n2 -> do
          print (n1 + n2)

getInt:: String -> IO Int
getInt enterMessage = do
  putStr enterMessage
  input <- getLine
  case readMaybe input of
    Nothing -> do
      putStrLn ("Error: invalid input: " ++ input)
      getInt enterMessage
    Just n -> return n
          
sumOfTwoInputs2 :: IO()
sumOfTwoInputs2 = do
  x <- getInt enterMessage
  y <- getInt enterMessage
  print(x + y)
  where enterMessage = "Enter a number: "

--
--sumOfManyInputs :: IO()
--sumOfManyInputs = do
--  n <- getInt "Enter N (number of numbers to sum up): "
--
--getInts :: Int -> IO [Int]
--getInts n = sequence'
--  (replicate n getInt)
--  
--sequence' :: [IO a] -> IO [a]
--sequence' = 
