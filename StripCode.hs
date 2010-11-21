module Main where

import Control.Monad
import Data.List
import System.Environment

main = do 
     innm:outnm:_ <- getArgs
     lns <- lines `fmap` readFile innm
     writeFile outnm $ unlines $ f lns



f (l:ls) | "\\begin{code}" `isPrefixOf` l = 
            f $ tail $ dropWhile (not . ("\\end{code}" `isPrefixOf`)) ls
         | otherwise = l:f ls
f [] = []