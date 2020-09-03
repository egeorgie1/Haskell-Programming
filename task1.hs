import Data.List
import Data.Tuple

main :: IO()
main = do
  print (calcLuhnChecksum 7992739871)
  
toArray :: Int -> [Int]
toArray n
  | n `div` 10 == 0  = [n]
  | otherwise        = toArray (n `div` 10) ++ [n `mod` 10]
  
doubleEvenpos :: [(Int, Int)] -> [Int]
doubleEvenpos []        = []
doubleEvenpos (x:xs)    
  | (snd x) `mod` 2 > 0 = (fst x):(doubleEvenpos xs) 
  | otherwise           = (2*(fst x)):(doubleEvenpos xs)
  
sumDigits :: Int -> Int
sumDigits x = sum (toArray x)
  
calcLuhnChecksum :: Int -> Int
calcLuhnChecksum n =  (9*(sum (map sumDigits (doubleEvenpos (zip (toArray n) [1..]))))) `mod` 10
