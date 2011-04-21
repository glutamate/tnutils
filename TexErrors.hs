module Main where

import Control.Monad
import Data.List
import System.Environment

main = interact iact
--  c <- getContents 
--  foo $ lines c

iact :: String -> String
iact = unlines . foo1 . lines

foo1 :: [String] -> [String]
--foo :: [String] -> IO ()
foo1 lns@(('!':l):_) = lns
foo1 (l:lns) | "Warning" `isInfixOf` l = l : foo1 lns
             | "Error" `isInfixOf` l = l : foo1 lns
             | otherwise = foo1 lns
foo1 [] = []

