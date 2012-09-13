import System( getArgs)
import System.IO

main :: IO ()
main = do
  z <- getLine
  case (funcky (read $ z :: Double)) of
    True -> putStrLn "YES"
    False -> putStrLn "NO"

funcky:: Double -> Bool
funcky n = func bInf bSup
          where 
             middle = n/2
             mid = sqrt (n*2)
             bInf:: Double
             bInf = fromInteger $ truncate mid
             bSup = bInf+1
             resu i s = n - (i*s)/2  
             func 0 _ = False
             func i s = case (resu i s)  of
                         x | (x<=0) -> func (i-1) (s-1)
                         x | (x>middle) -> False
                         x -> case (diff x) of
                               y | (y==0) -> True
                               otherwise -> func (i-1) (s-1)
             diff x = x - ((midd x * ((midd x)+1))/2)
             midd x =  fromInteger $ truncate $ sqrt $ x*2  
                         


