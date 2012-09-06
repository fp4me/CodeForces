import System.IO (getLine)
import Data.List(nub,intersect)

main :: IO ()
main = do
  z <- getLine
  print (numbers (read $ z :: Int))


primes:: Int -> [Int]
primes 1 = [1]
primes n = 1:(prime n 2) 
           where sqrtN num = truncate $ sqrt $ fromIntegral num 
                 prime num x | (x>(sqrtN num)) = [num]
                 prime num x = case ((divE*x)==num) of 
                  True ->  x: (prime divE (x)) 
                  False -> prime num (x+1)
                  where divE = div num x 

subsets:: [Int] -> [[Int]]
subsets [] = []
subsets (x:xs) = (xs : (map (x:) sub)) ++ sub   
                 where sub = nub $ map ((:[]).(foldr (*) 1)) $ subsets (xs)


getDigits n = case (divMod n 10) of
               (0,m) -> [m]
               (d,m) -> m : (getDigits d)

numbers z = length $ filter (\x -> (length (intersect (getDigits x) digitsN)>0)) temp  
           where
             temp =  nub $ (z:) $ concat $ subsets $  primes z 
             digitsN = getDigits z  

 

                  
               
