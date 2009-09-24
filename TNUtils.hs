module TNUtils where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as BS
import qualified Data.Binary as B
import System.Time
import Control.Concurrent
import Debug.Trace
import Data.List
import Control.Monad
import Data.Char
import System.Directory
import Data.Maybe

optVal :: Read a => Char -> a -> [String] -> a 
optVal key def opts = case find (['-', key] `isPrefixOf`) opts of
                        Nothing -> def
                        Just ('-':_:s) -> safeRead s `orJust` def

optStr :: Char -> String -> [String] -> String
optStr key def opts = case find (['-', key] `isPrefixOf`) opts of
                        Nothing -> def
                        Just ('-':_:s) -> s



cond :: [(Bool, a)] -> a -> a
cond [] x = x
cond ((True, x):_) _ = x
cond ((False, _):conds) x = cond conds x

nonempty = not . null

idInt :: Int -> Int
idInt = id

spliceFirst spl (s:ss) = (spl++s):ss

oneTrailingSlash "/" = "/"
oneTrailingSlash "" = ""
oneTrailingSlash s = case last s of
                      '/' -> s
                      _ -> s++"/"

noInitSlash ('/':s) = s
noInitSlash s = s

x ./ y = oneTrailingSlash x ++ noInitSlash y


uniqueFileInDir :: String -> String -> IO String
uniqueFileInDir base dir = do
  conts <- getDirectoryContents dir
  let fileNoMax = foldl (max) 0 $ catMaybes $ map (safeRead . (drop $ length base)) $ filter (base `isPrefixOf`) conts
  return $ dir ./ (base++show fileNoMax)

inChunksOf :: Int -> [a] -> [[a]]
inChunksOf _ [] = []
inChunksOf n xs = let (hds, tls) = splitAt n xs 
                  in hds : inChunksOf n tls

orJust (Just x) _ = x
orJust _ y = y 

loadBinaryStrict :: B.Binary w =>FilePath-> IO w
loadBinaryStrict fp = do c <- BS.readFile fp 
                         return $ B.decode $ L.fromChunks [c] 


saveBinary :: B.Binary w => FilePath-> w -> IO ()
saveBinary fp w = L.writeFile fp {-. compress-} . B.encode $ w --writeFile fp . show

appendBinary :: B.Binary w => FilePath-> w -> IO ()
appendBinary fp w = L.appendFile fp {-. compress-} . B.encode $ w --writeFile fp . show

loadBinary :: B.Binary w =>FilePath-> IO w
loadBinary fp = return . B.decode {-. decompress -}=<< L.readFile fp --readFile fp >>= return . read

lookupMany :: Eq a => a -> [(a,b)] -> [b]
lookupMany x = map snd . filter ((==x) . fst)

--http://www.serpentine.com/blog/2009/01/11/fun-with-haskell-view-patterns/
dropPrefix :: Eq a => [a] -> [a] -> ([a],[a])
dropPrefix left@(x:xs) right@(y:ys)
    | x == y    = dropPrefix xs ys
dropPrefix left right = (left,right)

unCap [] = []
unCap (c:cs) = toLower c : cs

beginsWithHyphen ('-':_) = True
beginsWithHyphen _ = False



for = flip map

safeHead [] = Nothing
safeHead (x:_) = Just x

safeLast [] = Nothing
safeLast xs = Just $ last xs

safeRead :: Read a => String -> Maybe a
safeRead x = case readsPrec 5 x of 
               [] -> Nothing
               (x,_):_ -> Just x
               


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

whenM mb ma = do b <- mb
                 when b ma

unlessM mb ma = do b <- mb
                   when (not b) ma


untilM :: Monad m => m Bool -> m () -> m ()
untilM mp ma = do p <- mp
                  if p 
                     then return ()
                     else ma >> untilM mp ma
                     

ifM :: Monad m => m Bool -> m a ->  m a -> m a
ifM mp mc ma = do p <- mp
                  if p
                     then mc
                     else ma


guardM :: MonadPlus m => m Bool -> m ()
guardM mb = mb >>= guard


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
