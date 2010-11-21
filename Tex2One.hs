module Main where

import Control.Monad
import Data.List
import System.Environment

main = fmap head getArgs >>= input


input :: String -> IO ()
input nm = fmap lines (readFile $ nm++".tex") >>= mapM_ f


f s | "\\input{" `isPrefixOf` s = 
        let nm = takeWhile (/='}') $ drop 7 s in input nm
    | otherwise = putStrLn s