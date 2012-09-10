import System.IO (getChar)
import Control.Monad (liftM)


main :: IO ()
main = do
         nbS <- getLine
         res <- swap' (read $ nbS :: Int) [] (One 0) None
         case res of 
          True -> putStrLn "YES"
          False -> putStrLn "NO"
 
getNumber =  do 
              s <- getNumber'
              case s of 
               [] -> return Nothing 
               otherwise -> return $ Just (read $ s :: Int) 

getNumber'  = do 
                c <- catch getChar  (\err-> return ' ') 
                case c of 
                  ' ' ->  return ""
                  '\n' ->  return ""
                  otherwise -> do l <- getNumber'
                                  return (c:l)

getNumbers n | (n<=0) = return []
getNumbers n = do
                mbNum <- getNumber 
                case mbNum of
                 Nothing -> return []
                 Just num -> (liftM (num:) (getNumbers(n-1)))


data Push = Value Int Int Int | None | Already | Switch Int Int Int 
data Last = One Int | Mul Int Int  

swap':: Int -> [Int] -> Last -> Push  -> IO Bool 
swap' n [] _ _   | (n<=0) = return True 
swap' n xs l p   | (n<=0) = swap n xs l p   
swap' n t l p   = do
                   nums <- getNumbers (3-(length t))
                   swap (n-length(nums)) (t++nums) l p  

swap:: Int -> [Int] -> Last -> Push  -> IO Bool 

swap u (x:y:[]) (One l)  None | x>y = return $ y>= l
swap u (x:y:[]) (Mul i l)  None | x>y = return $ y>= l

swap u (x:y:xs) (One l)  p | (x==y) = swap' u (y:xs) (Mul x l)  p  
swap u (x:y:xs) (Mul n l) p | (x==y) = case (x==n) of
                                      True -> swap' u (y:xs) (Mul n l) p 
                                      False -> swap' u (y:xs) (Mul x n) p 

swap u (x:y:xs) i  Already | x>y =  return False

swap u (x:y:xs) i None | x>y =  case (y>=(getLast i)) of
                               True -> case xs of
                                  []  -> return True  
                                  (z:zs) | z>=x -> swap' u (z:zs) (One x) $ Switch (getN i) y x
                                  (z:zs) | otherwise -> case i of
                                             Mul n _ ->  case (n==x) of
                                                          True -> return False
                                                          False -> swap' u (y:xs) i $ Value n x y 
                                             One n -> swap' u (y:xs) i $ Value n x y 
                               False -> return False
  
swap u (x:y:xs) i (Switch bInf a b) | x>y = return $  (b>=x)&&(y<=a)&&(y>=bInf)

swap u (x:y:xs) i p@(Value bInf v bSup) | x>y = case ((y>=bInf) && (y<=bSup) && (v>=(getN i))) of
                                               False -> return False
                                               True -> swap' u (v:xs) (One x) Already 

swap u (x:y:xs) (Mul n l) p = case (n==x) of
                              True -> swap' u (y:xs) (Mul n l) p
                              False ->  swap' u (y:xs) (One x) p

swap u (x:y:xs) i p = swap' u (y:xs) (One x) p

swap u (x:[]) i (Value bInf v bSup) = return $  ((bSup==(getN i))&&(v<=x)) || ((v>(getN i)) && (x>=bInf) && (x<= bSup))    
swap u (x:[]) i p = return $ (x>= (getN i))  

swap u ([]) i p = return True 

getN (One n) = n
getN (Mul n l) = n

getLast (One n) = n
getLast (Mul n l) = l

