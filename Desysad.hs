{-# LANGUAGE GADTs, ScopedTypeVariables #-} 
module Desysad where

import Control.Monad.State.Strict
import Control.Monad.Error
import System.IO
import System.Directory
import System.Process
import System.Exit
import Control.Concurrent
import Control.Exception as E

--- ONE APPROACH

data Cmd a where
  Return :: a -> Cmd a
  Bind :: Cmd a -> (a -> Cmd b) -> Cmd b
  Run :: String -> Cmd String
  Fail :: String -> Cmd a
  Mplus :: Cmd a -> Cmd a -> Cmd a

instance Functor Cmd where
  fmap f cmd = cmd `Bind` (Return . f)  

instance Monad Cmd where
   return = Return
   (>>=) = Bind
   fail = Fail

instance MonadPlus Cmd where
      mzero = Fail "mzero"
      mplus x y = Mplus x y

data DS = DS {
     pkgList1 :: [String],
     runTests1 :: Bool
  }

type Sys a =  StateT DS  Cmd a



runCmd :: Cmd a -> IO a
runCmd (Return x) = return x
runCmd (Fail s) = fail s
runCmd (Bind cmdx f) =  runCmd cmdx >>= runCmd . f 
runCmd (Run s) = do res <- sh s
                    case res of
                       Left s -> fail s
                       Right s -> return s
runCmd (Mplus (Fail s) cy) = runCmd cy
runCmd (Mplus cx cy) = do
       runCmd cx `E.catch` (\(e::SomeException)-> runCmd cy)

runSys :: Sys a -> IO a
runSys stcmda = do
    let s = DS [] False
    runCmd $ evalStateT stcmda s


run :: String -> Sys ()
run s = lift (Run s) >> return ()

ensure_pkg_list :: Sys ()
ensure_pkg_list = do 
      pkgs <- fmap pkgList1 get
      when (null pkgs) $ do
           return () 

has_apt :: String -> Sys ()
has_apt pkg = do
   run ("dpkg -l $1 >/dev/null 2>/dev/null") `mplus` run ("apt-get install -y -q $i")

--- ANOTHER ATTEMPT

data DS1= DS1 {
     handles :: (Handle, Handle, Handle, ProcessHandle),
     pkgList :: [String],
     runTests :: Bool

  }

type Sys1 a =  StateT DS1 (ErrorT String IO) a

io m = lift $  lift m

sh :: String -> IO (Either String String)
sh cmd = do (hin, hout, herr, ph) <- runInteractiveCommand cmd
            excode <-  waitForProcess ph
            sout <-  hGetContents hout
            serr <- hGetContents herr
            case excode of
                  ExitSuccess -> return $ Right sout
                  ExitFailure n ->
                      return $ Left $ concat ["process error ",
                                           show n,
                                           " :",
                                           serr
                                          ]

main2 = runLocal $ do 
   pwd <- cmd "pwd"
   console pwd

cmd :: String -> Sys1 String
cmd s = do (inp, o , e, p) <- fmap handles get
           io $ hPutStr inp $ s++"\n"
           io $ hGetLine o

console :: String -> Sys1 ()
console s = lift $ lift $ putStrLn s

--pwd :: Sys String
--pwd = 

runLocal :: Sys1 () -> IO ()
runLocal esio = do
--      hSetBuffering stdin NoBuffering   
      h@(inp,o,e,pid) <- runInteractiveCommand "bash" 
      threadDelay $ 100*1000
      myForkIO $ do let s = DS1 h [] False
                    let errio = evalStateT esio s
                    r <- runErrorT errio
                    case r of
                       Right x -> return x 
                       Left s -> error s
      hClose o
      hClose e
      hClose inp
      terminateProcess pid
      waitForProcess pid      
  --    bufmode <- hGetBuffering stdin
  --    when (bufmode /= LineBuffering) $ hSetBuffering stdin LineBuffering 
      return ()

myForkIO :: IO () -> IO ()
myForkIO io = do
     mvar <- newEmptyMVar
     forkIO (io `finally` putMVar mvar ())
     takeMVar mvar
