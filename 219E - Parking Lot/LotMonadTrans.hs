import System.IO (getChar)
import Control.Monad (liftM)
import Data.IntMap
import Data.Maybe (fromMaybe)
import Prelude hiding(lookup,null)
import Control.Monad.Trans(lift)
import Control.Monad.Trans.State

data Intv = Intv {getInf::Int,
                  getSup::Int
                 } deriving Show

data  Env = Env {
                 sizeLot::Int,
                 values::IntMap Int,
                 eIntvs::IntMap (IntMap Intv),
                 bIntvs::IntMap (Int,Intv)
                }  deriving Show


type LotState a = StateT Env IO a

data Pos = Pos Int Int | None deriving Show


data BIntv = BIntv (Int,Intv) | BNotFound 
 
main :: IO ()
main = do
         nbS <- getNumbers 2
         let size = nbS !! 0
         let nbActs = nbS !! 1
         runManage size nbActs >>= printRes
         
printRes (x:xs) = print x >> printRes xs
printRes [] = return ()

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


ins:: Int -> LotState Int
ins v = do 
          pos <- takePos 
          insertValue pos v >> return pos
        
insertValue pos v = do
                      s <- get
                      put $ s {values = insert v pos $ values s}    

takeValue v = do
                   s <- get
                   let curValues = values s 
                   let pos = curValues ! v 
                   put $ s {values = delete v curValues}
                   return pos  


deleteInEIntvs bInf e = do
                      env <- get
                      let curEIntvs = eIntvs env
                      let newBIntvs = delete bInf $ bIntvs env
                      let intvs = curEIntvs ! e
                      let newIntvs = delete bInf intvs
                      if (null newIntvs)
                        then put $ env {eIntvs = delete e curEIntvs,bIntvs=newBIntvs} 
                        else put $ env {eIntvs = insert e newIntvs curEIntvs,bIntvs=newBIntvs}    
 
updateEIntvs m = do 
                   s <- get
                   put $ s {eIntvs = m}

insertEIntvs e v = do 
                    s <- get
                    let curEIntvs = eIntvs s
                    put $ s {eIntvs = insert e v curEIntvs}
                 
updateBIntvs m = do 
                   s <- get
                   put $ s {bIntvs = m}

insertBIntvs bInf v = do
                        s <- get
                        put s {bIntvs = insert bInf v $ bIntvs s}

takePos:: LotState Int
takePos = do
            s <- get
            let curEIntvs = eIntvs s
            let curBIntvs = bIntvs s

            let (e,maxIntvs) = findMax curEIntvs
            let (minBInf,intv) = findMin maxIntvs
            
            let newBIntvs = delete minBInf curBIntvs
            updateBIntvs newBIntvs
            let sz = sizeLot s
            let newIntvs = delete minBInf maxIntvs
            let newEIntvs = if (null newIntvs) 
                               then delete e curEIntvs
                               else insert e newIntvs curEIntvs
            
            updateEIntvs newEIntvs

            case intv of
             (Intv bInf bSup) | (bInf==bSup) ->  return bInf
             (Intv 1 bSup) | (bSup==sz) -> insertIntv (e-1) (Intv 2 sz) >> return 1  --ok 
             (Intv 1 bSup) -> insertIntv (calcEcart 2 bSup) (Intv 2 bSup) >> return 1   --ok
             (Intv bInf bSup) | (bSup==sz) -> insertIntv (calcEcart bInf (sz-1)) (Intv bInf (sz-1)) >> return sz --ok
             (Intv bInf bSup) | e==0 -> insertIntv 0 (Intv bSup bSup) >> return bInf --ok
             (Intv bInf bSup) -> do
                                   let pos = bInf+e
                                   let eFirst = calcEcart bInf (pos-1)
                                   let eSecond = calcEcart (pos+1) bSup
                                   insertIntv eFirst $ Intv bInf (pos-1) 
                                   insertIntv eSecond $ Intv (pos+1) bSup 
                                   return pos

            where calcEcart bInf bSup = truncate $ (diff bInf bSup) / 2
                  diff bInf bSup = fromIntegral $ bSup-bInf 

insertIntv e v = do
                   s <- get
                   let bInf = getInf v 
                   let curEIntvs = eIntvs s 
                   insertBIntvs bInf (e,v)                     
                   case (lookup e curEIntvs) of
                     Just intvs -> insertEIntvs e (insert bInf v intvs)
                     Nothing -> insertEIntvs e (singleton bInf v)    

insFusion bInf bSup = do
                        env <- get
                        let sz = sizeLot env
                        let  e = case (bInf,bSup) of
                                 (1,b) -> b-1
                                 (a,b) | (b==sz) -> sz-a 
                                 (a,b) -> let diff = fromIntegral (b-a)
                                          in truncate $ diff / 2 
                        insertIntv e (Intv bInf bSup)

remV v = do
          pos <-  takeValue v 
          remPos pos

remPos p = do
             env <- get
             let sz = sizeLot env
             let curBIntvs = bIntvs env
             let (mins,_) = split p curBIntvs                             
             let bInf = if (p==1) 
                         then BNotFound
                         else if null mins
                               then BNotFound
                               else let (_,(e,i)) = (findMax mins)
                                    in if ((getSup i)==(p-1))
                                      then  BIntv (e,i)
                                      else BNotFound
                       

             let bSup = if (p==sz) 
                   then BNotFound
                   else fromMaybe BNotFound $ do
                            (e,i) <- lookup (p+1) curBIntvs
                            return $ BIntv (e,i)                        
  
             case (bInf,bSup) of
                (BIntv (eInf,iInf),BIntv (eSup,iSup)) -> do
                                                           deleteInEIntvs (getInf iInf) eInf
                                                           deleteInEIntvs (getInf iSup) eSup  
                                                           insFusion (getInf iInf) (getSup iSup)
                                        
                (BIntv (eInf,iInf),_) -> do
                                           deleteInEIntvs (getInf iInf) eInf
                                           insFusion (getInf iInf) p

                (_,BIntv (eSup,iSup)) -> do
                                           deleteInEIntvs (getInf iSup) eSup
                                           insFusion p (getSup iSup)
                otherwise ->  insFusion p p     


initEnv n = Env {sizeLot=n,
                 values = empty,
                 bIntvs = singleton 1 ((n-1),(Intv 1 n)), 
                 eIntvs = singleton (n-1) (singleton 1 (Intv 1 n))
                }

getState n xs = execStateT (test xs)  (initEnv n)  
testState n xs = evalStateT (test xs)  (initEnv n)  

test [] = return []
test (action:xs) = do
                     let value = snd action
                     case (fst action ) of
                          1 -> do
                                pos <- ins value
                                liftM (pos:) $  test xs
                          2 -> remV value >> test xs     

runManage n nbActs = evalStateT (manage nbActs) (initEnv n)

manage 0 = return []
manage nbActs= do
                  action <- lift $ getNumbers 2
                  let value = action !! 1
                  case (action !! 0) of

                          1 -> do
                                pos <- ins value                                
                                liftM (pos:) $  manage (nbActs-1) 
                          2 -> remV value >> manage (nbActs-1)     


                            
