module Main where

import Control.Monad
import Data.List
import System.Environment

main = do 
     fmap f getContents >>= putStr


f ('$':'|':ls) =  '|': f ls
f ('|':'$':ls) =  '|': f ls
f (l:ls) = l : f ls
f [] = []
