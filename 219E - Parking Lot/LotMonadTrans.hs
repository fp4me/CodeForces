import System.IO (getChar)
import Control.Monad (liftM)
import Data.IntMap
import Data.Maybe (fromMaybe)
import Prelude hiding(lookup,null)
--import Control.Monad.Trans(lift)
--import Control.Monad.Trans.State
import Data.Array.IO
import Data.Array.Unboxed hiding ((!))

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Monad m) => Functor (StateT s m) where
	fmap f m = StateT $ \s -> do
		(x, s') <- runStateT m s
		return (f x, s')

instance (Monad m) => Monad (StateT s m) where
	return a = StateT $ \s -> return (a, s)
	m >>= k  = StateT $ \s -> do
		(a, s') <- runStateT m s
		runStateT (k a) s'
	fail str = StateT $ \_ -> fail str

evalStateT :: (Monad m) => StateT s m a -> s -> m a
evalStateT m s = do
	(a, _) <- runStateT m s
	return a


execStateT :: (Monad m) => StateT s m a -> s -> m s
execStateT m s = do
	(_, s') <- runStateT m s
	return s'


lift m = StateT $ \s -> do
		a <- m
		return (a, s)

get   = StateT $ \s -> return (s, s)
put s = StateT $ \_ -> return ((), s)


data Intv = Intv {getInf::Int,
                  getSup::Int
                 } deriving Show

data  Env = Env {
                 sizeLot:: Int,
                 values:: IOUArray Int Int,
                 eIntvs:: IntMap (IntMap Intv),
                 bInfs:: IOUArray Int Int,
                 bSups:: IOUArray Int Int
                }  


type LotState a = StateT Env IO a

data BIntv = BIntv (Int,Intv) | BNotFound 
 
main :: IO ()
main = do
         nbS <- getNumbers 2
         let size = nbS !! 0
         let nbActs = nbS !! 1
         runManage size nbActs 
         
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
                      lift $ writeArray (values s) v pos 

takeValue v = do
                   s <- get
                   let curValues = values s 
                   pos <- lift $ readArray curValues v
                   return pos  


deleteInEIntvs b e = do
                      env <- get
                      let curEIntvs = eIntvs env
                      let bInf = getInf b
                      let bSup = getSup b
                      
                      lift $ writeArray (bInfs env) bInf (-1)
                      lift $ writeArray (bSups env) bSup (-1)
                     
                      let intvs = curEIntvs ! e
                      let newIntvs = delete bInf intvs
                      if (null newIntvs)
                        then put $ env {eIntvs = delete e curEIntvs} 
                        else put $ env {eIntvs = insert e newIntvs curEIntvs}
                                          
insertEIntvs e v = do 
                    s <- get
                    let curEIntvs = eIntvs s
                    put $ s {eIntvs = insert e v curEIntvs}

updateEIntvs m = do 
                   s <- get
                   put $ s {eIntvs = m}
                 
deleteBIntvs (Intv bInf bSup)= do
                   s <- get
                   lift $ writeArray (bInfs s) bInf (-1)
                   lift $ writeArray (bSups s) bSup (-1)

insertBIntvs (Intv bInf bSup) = do
                                s <- get
                                lift $ writeArray (bInfs s) bInf bSup
                                lift $ writeArray (bSups s) bSup bInf

takePos:: LotState Int
takePos = do
            s <- get
            let curEIntvs = eIntvs s

            let (e,maxIntvs) = findMax curEIntvs
            let (intv,newIntvs) = deleteFindMin maxIntvs

            deleteBIntvs intv 

            let sz = sizeLot s
            let newEIntvs = if (null newIntvs) 
                               then delete e curEIntvs
                               else insert e newIntvs curEIntvs
            
            updateEIntvs newEIntvs

            case intv of
             (Intv bInf bSup) | (bInf==bSup) ->  return bInf
             (Intv 1 bSup) | (bSup==sz) -> insertIntv (sz-2) (Intv 2 sz) >> return 1  --ok 
             (Intv 1 bSup) -> insertIntv (calcE 2 bSup) (Intv 2 bSup) >> return 1   --ok
             (Intv bInf bSup) | (bSup==sz) -> insertIntv (calcE bInf (sz-1)) (Intv bInf (sz-1)) >> return sz --ok
             (Intv bInf bSup) | e==0 -> insertIntv 0 (Intv bSup bSup) >> return bInf --ok
             (Intv bInf bSup) -> do
                                   let pos = bInf+e
                                   let eFirst = calcE bInf (pos-1)
                                   let eSecond = calcE (pos+1) bSup
                                   insertIntv eFirst $ Intv bInf (pos-1) 
                                   insertIntv eSecond $ Intv (pos+1) bSup 
                                   return pos
            where calcE bInf bSup = truncate $ (diff bInf bSup) / 2
                  diff bInf bSup = fromIntegral $ bSup-bInf 

calcEcart bInf bSup = do
                       s <- get
                       let sz = sizeLot s
                       case (bInf,bSup) of
                        (1,bS) | (bS==sz) -> return (sz)
                        (1,bS) -> return (bS-1)
                        (bI,bS) | (bS==sz) -> return (sz-bI)
                        (bI,bS) | (bI==bS) -> return 0
                        otherwise -> return $ truncate $ (diff bInf bSup) / 2
                       where diff bInf bSup = fromIntegral $ bSup-bInf

insertIntv e v = do
                   s <- get
                   let bInf = getInf v 
                   let bSup = getSup v 
                   let curEIntvs = eIntvs s 
                   insertBIntvs v                     
                   insertBIntvs v                     
                   case (lookup e curEIntvs) of
                     Just intvs -> insertEIntvs e (insert bInf v intvs)
                     Nothing -> insertEIntvs e (singleton bInf v)    

insFusion bInf bSup = do
                        env <- get
                        let sz = sizeLot env
                        let  e = case (bInf,bSup) of
                                 (1,b) | ( b==sz) -> b
                                 (1,b) -> b-1
                                 (a,b) | (b==sz) -> sz-a 
                                 (a,b) -> let diff = fromIntegral (b-a)
                                          in truncate $ diff / 2 
                        insertIntv e (Intv bInf bSup)

remV v = do
          pos <- takeValue v 
          remPos pos

remPos p = do
             env <- get
             let sz = sizeLot env
             let curBInfs = bInfs env
             let curBSups = bSups env

             mBinf <- if (p==1)
                        then return Nothing
                        else do 
                               bInfinf <- lift $ readArray curBSups (p-1)
                               if (bInfinf<0)
                                then return Nothing
                                else do
                                      e <- calcEcart bInfinf (p-1)
                                      return $ Just $ BIntv (e,(Intv bInfinf (p-1)))

             let bInf = fromMaybe BNotFound mBinf -- >>= return.BIntv)

             mBintvSup <- if(p==sz) 
                           then return Nothing
                           else do
                                 bSupsup <- lift $ readArray curBInfs (p+1)
                                 if (bSupsup<0)
                                  then return Nothing
                                  else do
                                        e <- calcEcart (p+1) bSupsup
                                        return $ Just $ BIntv (e, (Intv (p+1) bSupsup))

             let bSup = fromMaybe BNotFound mBintvSup -- >>= return.BIntv) 
  
             case (bInf,bSup) of
                (BIntv (eInf,iInf),BIntv (eSup,iSup)) -> do
                                                           deleteInEIntvs iInf eInf
                                                           deleteInEIntvs iSup eSup  
                                                           insFusion (getInf iInf) (getSup iSup)
                                        
                (BIntv (eInf,iInf),_) -> do
                                           deleteInEIntvs iInf eInf
                                           insFusion (getInf iInf) p

                (_,BIntv (eSup,iSup)) -> do
                                           deleteInEIntvs iSup eSup
                                           insFusion p (getSup iSup)
                otherwise ->  insFusion p p     


initEnv n = do
              values <- newArray (1,1000000) 0
              bInfs <- newArray (1,n) (-1)
              bSups <- newArray (1,n) (-1)
              writeArray bInfs 1 n
              writeArray bSups n 1
              return $ Env {sizeLot=n,
                 values = values,
                 bInfs = bInfs, 
                 bSups = bSups, 
                 eIntvs = singleton (n-1) (singleton 1 (Intv 1 n))
                }

test [] = return []
test (action:xs) = do
                     let value = snd action
                     case (fst action ) of
                          1 -> do
                                pos <- ins value
                                liftM (pos:) $  test xs
                          2 -> remV value >> test xs     

runManage n nbActs = do
                       env <- initEnv n
                       evalStateT (manage nbActs) env



manage 0 = return ()
manage nbActs= do
                  action <- lift $ getNumbers 2
                  let value = action !! 1
                  case (action !! 0) of

                          1 -> do
                                pos <- ins value                                
                                (lift $ print pos) >> manage (nbActs-1) 
                          2 -> remV value >> manage (nbActs-1)     


                            
