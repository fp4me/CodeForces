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


data Push = Value Int Int Int | None | Already 
data Last = One Int | Mul Int Int  

swap:: Int -> [Int] -> Last -> Push  -> IO Bool 

swap':: Int -> [Int] -> Last -> Push  -> IO Bool 
swap' n [] _ _   | (n<=0) = return True 
swap' n xs l p   | (n<=0) = swap n xs l p   
swap' n t l p   = do
                   nums <- getNumbers (2-(length t))
                   swap (n-length(nums)) (t++nums) l p   

swap u (x:y:xs) (One l)  p | (x==y) = swap' u (y:xs) (Mul x l)  p  
swap u (x:y:xs) (Mul n l) p | (x==y) = case (x==n) of
                                      True -> swap' u (y:xs) (Mul n l) p 
                                      False -> swap' u (y:xs) (Mul x n) p 

swap u (x:y:xs) p  Already | x>y =  return False
 
swap u (x:y:xs) last@(One l) None | x>y =  case (y>= l) of
                                    True -> swap' u (y:xs) last $ Value l x y 
                                    False -> return False


swap u (x:y:xs) last@(One l) (Value bInf v bSup) | x>y =  case  ((x<=v) && (bInf<=y) && (y<=bSup)) of
                                                  True ->  swap' u (v:xs) (One x) Already  
                                                  False ->  return False  
 
swap u (x:y:xs) (Mul n l) None | x>y = case (y >= n) of
                                           True -> swap' u (y:xs) (Mul n l) $ Value n x y 
                                           False -> case ((y>=l) && (y<x)) of
                                            True -> swap' u (x:xs) (Mul n l) Already
                                            False -> return False    

swap u (x:y:xs) (Mul n l) (Value bInf v bSup ) | x>y =  case  ((x<=v) && (bInf<=y) && (y<=bSup)) of
                                                  True ->  swap' u  (v:xs) (Mul n l) Already  
                                                  False ->  return False

swap u (x:y:xs) i@(One l) p@(Value bInf v bSup) = case (v<y) of
                                        False -> swap' u (y:xs) (One x) p
                                        True -> case (((x>=bInf) && ( x<=bSup)) || ((l==bSup) && (v==x))) of
                                          True -> swap' u (v:y:xs) i Already
                                          False -> return False

swap u (x:y:xs) i@(Mul n l) p@(Value bInf v bSup) = case (v<y) of
                                        False -> swap' u (y:xs) (One x) p
                                        True -> case ( ((x>=bInf) && ( x<=bSup)) || ((l==bSup) && (v==n))) of
                                          True -> swap' u (v:y:xs) i Already
                                          False -> return False 

swap u (x:y:xs) (Mul n l) p = case (n==x) of
                              True -> swap' u (y:xs) (Mul n l) p
                              False ->  swap' u (y:xs) (One x) p

swap u (x:y:xs) i p = swap' u (y:xs) (One x) p

swap u (x:[]) (Mul n l) (Value bInf v bSup) = return $ (l<=bSup) && (v>=n)
swap u (x:[]) (One l) (Value bInf v bSup) = return $ ((x>=v) && (bSup==l)) || ((v>=l)&&(x>=bInf)&&(x<=bSup))
swap u (x:[]) i p = case p of
                   (Value bInf v bSup) -> return $  (v>=(getN i))&&(x>=bInf)&&(x<=bSup)
                   otherwise -> return $ (x>= (getN i))  

swap u ([]) i p = return True 

getN (One n) = n
getN (Mul n l) = n
 

