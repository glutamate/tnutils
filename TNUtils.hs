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
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer hiding (tell)
import qualified Control.Monad.Writer as W
import System.IO


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

strictWriteBS :: String -> BS.ByteString -> IO ()
strictWriteBS fnm bs = do
  h <- openBinaryFile fnm WriteMode
  BS.hPut h bs
  hClose h

loadBinaryStrict :: B.Binary w =>FilePath-> IO w
loadBinaryStrict fp = do c <- BS.readFile fp 
                         return $ B.decode $ L.fromChunks [c] 

writeBinary :: B.Binary w => Handle-> w -> IO ()
writeBinary h = L.hPut h . B.encode


saveBinary :: B.Binary w => FilePath-> w -> IO ()
saveBinary fp w = L.writeFile fp {-. compress-} . B.encode $ w --writeFile fp . show

saveBinaryStrict :: B.Binary w => FilePath-> w -> IO ()
saveBinaryStrict fp w = strictWriteBS fp {-. compress-} . BS.concat . L.toChunks $ B.encode $ w --writeFile fp . show


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

onBoth f (x,y) = (f x, f y)



fst3 (x,_,_) = x
snd3 (_,x,_) = x
trd3 (_,_,x) = x

fst4 (x,_,_,_) = x
snd4 (_,x,_,_) = x
trd4 (_,_,x,_) = x
fth4 (_,_,_,x) = x


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

mapM2 :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
mapM2 f xs ys = mapM (uncurry f) $ zip xs ys



maybeM :: Monad m => Maybe a -> (a -> m b) -> m ()
maybeM Nothing _ = return ()
maybeM (Just x) a = a x >> return ()
         
all2 :: (a->Bool) -> [[a]] -> Bool
all2 p = and . map (all p)

zip2d :: [[a]] -> [[b]] -> [(a,b)]
zip2d xss yss = concat $ zipWith zip xss yss

last3 [x,y,z] = [x,y,z]
last3 [] = []
last3 (x:xs) = last3 xs

euler :: Double -> Double -> Double -> (Double -> Double) -> Double
euler h t1 t2 f = let ts = [t1, t1+h..t2]
                      g acc t =  acc+h*f(t) 
                  in foldl g 0 ts


setIdx :: Int -> a-> [a]->[a]
setIdx _ _ [] = []
setIdx 0 x (_:ys) = x:ys
setIdx n x (y:ys) = y : setIdx (n-1) x ys

forIdx ::  [a] -> (Int ->  b) ->[b]
forIdx xss f = map (f . snd) $ zip xss [0..]

forIdx2 :: [[a]] -> (Int -> Int -> b) -> [[b]]
forIdx2 xss f = map g $ zip xss [0..]
    where g (xs, i) = map  (f i . snd) $ zip xs [0..]
 
for2 :: [[a]] -> (a->b) -> [[b]]
for2 xss f = map (map f) xss 

{-globalSecsNow :: IO Double
globalSecsNow = do tnow <- getClockTime
                   tstart <- readTV globalTimeStartTVar
                   return $ diffInS tnow tstart   -}    

tic = getClockTime
toc t1 = do t2 <- getClockTime
            return $ diffInS t2 t1

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

splitByMany :: Eq a => [a] -> [a] -> [[a]]
splitByMany _ [] = []
splitByMany p cs | p `isPrefixOf` cs = splitByMany p (drop (length p) cs)
                 | otherwise = case posOfInfix p cs of
                                 Nothing -> [cs]
                                 Just n -> take n cs : splitByMany p (drop n cs)
    where posOfInfix p [] = Nothing
          posOfInfix p s | p `isPrefixOf` s = Just 0
                         | otherwise = (+1) `fmap` posOfInfix p (tail s)

beforeInfix p [] = []
beforeInfix p s | p `isPrefixOf` s = []
                | otherwise = (head s) : beforeInfix p (tail s)

testSplit = splitBy ' ' "foo bar"


eqSub [] _  =True
eqSub _ [] = False
eqSub (x:xs) (y:ys) | x == y = eqSub xs ys
                    | otherwise = False

substitute :: Eq a => [a] -> [a] -> [a] -> [a]
substitute old new = 
    let n = length old
        f [] = []
        f s@(c:cs) | eqSub old s = new++f (drop n s)
                   | otherwise = c : f cs
    in f

roundToFrac dt t = (realToFrac $ round $ t/dt)*dt


type CodeWriterT m a= StateT (Int, [String]) (WriterT [String] m) a

indent n = do 
  modify $ \(ind,ss)->(ind+n,ss)

indentAbs n = do 
  modify $ \(_,ss)->(n,ss)


tell :: Monad m => String -> CodeWriterT m ()
tell s = do n <- fst `fmap` get
            lift $ W.tell [(replicate n ' ')++s]

execCodeWriterT :: Monad m => String -> CodeWriterT m () -> m String
execCodeWriterT modNm cw = do
  ss <- execWriterT $ execStateT cw (0,[])
  let (imps, rest) = partition ("import " `isPrefixOf`) ss
  return $ unlines (("module "++modNm++" where"):imps++rest)

execCodeWriterNotHsT :: (Functor m, Monad m) => CodeWriterT m () -> m String
execCodeWriterNotHsT cw = do
  unlines `fmap` (execWriterT $ execStateT cw (0,[]))


thin :: Int -> [a] -> [a]
thin n [] = []
thin 0 xs  = xs
thin n xs = let (x:_, ys) = splitAt (n+1) xs
            in x : thin n ys
            
