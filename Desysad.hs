{-# LANGUAGE GADTs #-} 
module Desysad where

import Control.Monad.State.Strict
import Control.Monad.Error
import System.IO
import System.Directory
import System.Process
import System.Exit
import Control.Concurrent
import Control.Exception

--- ONE APPROACH

data Cmd a where
  Return :: a -> Cmd a
  Bind :: Cmd a -> (a -> Cmd b) -> Cmd b
  Run :: String -> Cmd String
  Fail :: String -> Cmd a

instance Functor Cmd where
  fmap f cmd = cmd `Bind` (Return . f)  

instance Monad Cmd where
   return = Return
   (>>=) = Bind
   fail = Fail

data DS1 = DS1 {
     pkgList1 :: [String],
     runTests1 :: Bool
  }

type Sys1 a =  StateT DS Cmd a



--- ANOTHER ATTEMPT

data DS = DS {
     handles :: (Handle, Handle, Handle, ProcessHandle),
     pkgList :: [String],
     runTests :: Bool

  }

type Sys a =  StateT DS (ErrorT String IO) a

io m = lift $  lift m

sh :: String -> IO String
sh cmd = do (hin, hout, herr, ph) <- runInteractiveCommand cmd
            excode <-  waitForProcess ph
            sout <-  hGetContents hout
            serr <- hGetContents herr
            case excode of
                  ExitSuccess -> return sout
                  ExitFailure n ->
                      return $ concat ["process error ",
                                           show n,
                                           " :",
                                           serr
                                          ]

main1 = runLocal $ do 
   pwd <- cmd "pwd"
   console pwd

cmd :: String -> Sys String
cmd s = do (inp, o , e, p) <- fmap handles get
           io $ hPutStr inp $ s++"\n"
           io $ hGetLine o

console :: String -> Sys ()
console s = lift $ lift $ putStrLn s

--pwd :: Sys String
--pwd = 

runLocal :: Sys () -> IO ()
runLocal esio = do
--      hSetBuffering stdin NoBuffering   
      h@(inp,o,e,pid) <- runInteractiveCommand "bash" 
      threadDelay $ 100*1000
      myForkIO $ do let s = DS h [] False
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
