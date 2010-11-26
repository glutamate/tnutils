module Main where

import Control.Monad
import Data.List
import System.Environment

main = do 
  c <- getContents 
  foo $ lines c

foo :: [String] -> IO ()
foo lns@(('!':l):_) = mapM_ putStrLn lns
foo (l:lns) | "Warning" `isInfixOf` l = putStrLn l >> foo lns
            | "Error" `isInfixOf` l = putStrLn l >> foo lns
            | otherwise = foo lns
foo [] = return ()


input :: String -> IO ()
input nm = fmap lines (readFile $ nm++".tex") >>= mapM_ f


f s | "\\input{" `isPrefixOf` s = 
        let nm = takeWhile (/='}') $ drop 7 s in input nm
    | otherwise = putStrLn s