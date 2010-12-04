module Main where

import TNUtils
import Data.List

main = do
  mods <- lines `fmap` sh "lsmod"
  if any ("psmouse" `isPrefixOf`) mods
     then sh "modprobe -r psmouse"
     else sh "modprobe psmouse"
  return ()