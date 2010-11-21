module Main where

import Control.Monad
import Data.List
import System.Environment

main = do 
     innm:outnm:_ <- getArgs
     lns <- lines `fmap` readFile innm
     writeFile outnm $ unlines $ f lns



f (l:ls) | "\\begin{tabbing}" `isPrefixOf` l = 
            f $ onHead (drop (length "\\end{tabbing}")) $ dropWhile (not . ("\\end{tabbing}" `isPrefixOf`)) ls
         | otherwise = l:f ls
f [] = []

onHead :: (a-> a) -> [a] -> [a]
onHead f [] =[]
onHead f (x:xs) = f x : xs