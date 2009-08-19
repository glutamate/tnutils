module TNUtils where

import qualified Data.ByteString.Lazy as L
import qualified Data.Binary as B
import System.Time
import Control.Concurrent
import Debug.Trace
import Data.List

cond :: [(Bool, a)] -> a -> a
cond [] x = x
cond ((True, x):_) _ = x
cond ((False, _):conds) x = cond conds x

nonempty = not . null

idInt :: Int -> Int
idInt = id

oneTrailingSlash "/" = "/"
oneTrailingSlash "" = ""
oneTrailingSlash s = case last s of
                      '/' -> s
                      _ -> s++"/"

inChunksOf :: Int -> [a] -> [[a]]
inChunksOf _ [] = []
inChunksOf n xs = let (hds, tls) = splitAt n xs 
                  in hds : inChunksOf n tls

orJust (Just x) _ = x
orJust _ y = y 


saveBinary :: B.Binary w => FilePath-> w -> IO ()
saveBinary fp w = L.writeFile fp {-. compress-} . B.encode $ w --writeFile fp . show

appendBinary :: B.Binary w => FilePath-> w -> IO ()
appendBinary fp w = L.appendFile fp {-. compress-} . B.encode $ w --writeFile fp . show

loadBinary :: B.Binary w =>FilePath-> IO w
loadBinary fp = return . B.decode {-. decompress -}=<< L.readFile fp --readFile fp >>= return . read


for = flip map

safeHead [] = Nothing
safeHead (x:_) = Just x

safeLast [] = Nothing
safeLast xs = Just $ last xs

maxIdx :: Ord a => [a] -> Int
maxIdx (x:xs) = mxIxAcc 1 0 x xs
    where mxIxAcc curI mI mV [] = mI
          mxIxAcc curI mI mV (x:xs) = if x>mV
                                        then mxIxAcc (curI+1) curI x xs
                                        else mxIxAcc (curI+1) mI mV xs

pair a b = (a,b)

onFst :: (a->b) -> (a,c)->(b,c)
onFst f (x,y) = (f x, y)

onSnd :: (a->b) -> (c,a)->(c,b)
onSnd f (x,y) = (x, f y)

fst3 (x,_,_) = x
snd3 (_,x,_) = x
trd3 (_,_,x) = x


maybeM :: Monad m => Maybe a -> (a -> m b) -> m ()
maybeM Nothing _ = return ()
maybeM (Just x) a = a x >> return ()
         
{-globalSecsNow :: IO Double
globalSecsNow = do tnow <- getClockTime
                   tstart <- readTV globalTimeStartTVar
                   return $ diffInS tnow tstart   -}    

diffInS (TOD t1s t1ps) (TOD t2s t2ps) = (fromInteger $ (t1s-t2s)*1000*1000 + ((t1ps-t2ps) `div` (1000*1000))) / 1000000

waitSecs :: Double -> IO ()
waitSecs s = threadDelay . round $ s*1000*1000


waitUntil t0 s = do tn <- getClockTime
                    let elapsed = diffInS tn t0
                    --print elapsed
                    if elapsed < s
                       then threadDelay . round $ (s-elapsed)*1000*1000
                       else return () 
 
secsSince t0 =  do tn <- getClockTime
                   return $  diffInS tn t0

print2 x y = putStrLn $ x++show y

traceM s = trace s $ return ()
traceM2 s s1 = trace (s++show s1) $ return ()

trace2 s s1 = trace (s++show s1)

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy p [] = []
splitBy p s@(c:cs) | c == p = splitBy p cs
                   | otherwise = let (hd, tl) = break (==p) s 
                                 in hd : splitBy p tl

testSplit = splitBy ' ' "foo bar"