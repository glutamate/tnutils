{-# LANGUAGE PackageImports #-}

module Main where

import Text.BibTeX.Entry
import System.Environment
import Data.Maybe
import Data.List
import Data.Char
import qualified Text.BibTeX.Parse as P
import Text.BibTeX.Entry
import Text.ParserCombinators.Parsec as Parsec
import TNUtils
type Key = String

data CiteBlock = CiteBlock { inparan:: Bool, keys:: [Key], littxt :: String } deriving Show

type Style = [CiteBlock] -> [T] -> ([String], String)

swap (x,y) = (y,x)

jrsi :: Style
jrsi citeblocks refdb = (intxt, ref) where
   keyNums = zip (nub $ concatMap keys citeblocks) [(1::Int)..]
   intxt = map mkInTxt citeblocks
   ref = "\\begin{thebibliography}{99}\n"++concatMap mkRef keyNums++"\\end{thebibliography}\n\n"
   getNum key = fromJust $ lookup key $ keyNums
   mkInTxt (CiteBlock _ ks _) = compressShow (map getNum ks)
   mkRef (key, num) = "\\bibitem{"++key++"}"++format key++"\n\n"
   format key = case [ formatByTy (map toLower refty) flds | Cons refty k flds <- refdb, key==k ] of
          [] -> error $ "notfound key "++key
          [[]] -> error $ "error in key "++key
          (x:_):_ -> x
   getF f flds = case [ s | (fl, s) <- flds, fl==f] of
                  [] -> ""
                  s:_ -> s
   auFormat s = let aus = [ lastnm++", "++intercalate " " (map ((++".") . take 1) firsts) 
                      | (lastnm, firsts) <- parseAuthors s ]
                 in if length aus > 10
                       then intercalate ", " (take 10 aus) ++ ", \\emph{et al.}"
                       else grammaticalList aus 
   doiStr flds = maybe "" (\doi->" (DOI "++doi++")") $ lookup "doi" flds
   edStr flds = maybe ". " (\ed->", "++ed++" ed. ") $ lookup "edition" flds 
   ppStr flds = maybe "" (\pp->", "++pp) $ lookup "pages" flds 
   formatByTy "article" flds = 
      [ auFormat au++" "++yr++" "++noFinalDot ti++". \\emph{"++jr++"} "++getF "volume" flds++ppStr flds++ doiStr flds| 
           ("author",au) <- flds,
           ("title",ti) <- flds,
           ("year",yr) <- flds,
           ("journal",jr) <- flds
        ]
   formatByTy "book" flds = 
      [ auFormat au++" "++yr++" \\emph{"++ti++"}"++edStr flds++loc++": "++pub | 
           ("author",au) <- flds,
           ("title",ti) <- flds,
           ("year",yr) <- flds,
           ("publisher",pub) <- flds,
           ("address",loc) <- flds
        ]
   formatByTy "inbook" flds = 
      [ auFormat au++" "++yr++" "++ti++". In \\emph{"++bkti++"}"
        ++"(ed. "++auFormat eds++"), pp. "++pgs++". "++loc++": "++pub | 
           ("author",au) <- flds,
           ("title",ti) <- flds,
           ("year",yr) <- flds,
           ("booktitle",bkti) <- flds,
           ("editor",eds) <- flds,
           ("pages",pgs) <- flds,
           ("publisher",pub) <- flds,
           ("address",loc) <- flds
        ]
   formatByTy "techreport" flds = 
      [ auFormat au++" "++yr++" "++ti++" "++pub | 
           ("author",au) <- flds,
           ("title",ti) <- flds,
           ("year",yr) <- flds,
           ("series",pub) <- flds
        ]
--   formatByTy ft flds = error $ "formatByTy: "++ ft
noFinalDot s | last s == '.' = init s
             | ".}" `isSuffixOf` s = take (length s -2) s ++ "}"
             | otherwise = s     
main = do
  -- first pass: load file, get citeBlocks
  fnm:bib:_ <- getArgs
  cblocks <- fmap pass1 $ readFile fnm
  --mapM print cblocks
  -- load bibliography db
  refdb <- parseBib =<< readFile bib 
  let (incites, ref) = jrsi cblocks refdb
--  mapM print $ take 10 refdb
  pass2 incites ref =<< readFile fnm
  --print $ parseAuthors "Mitchell, Simon J and Silver, R Angus"
  --print $ splitByMany " and " "Mitchell, Simon J and Silver, R Angus"
  --print $ splitByOneOf "., " "R Angus"
  return ()
   
  -- second pass: replace citeblocks

compressShow :: [Int]-> String
compressShow xs@[x,y,z] 
   | succ2 x y && succ2 y z = "["++show x++"-"++ show z++"]"
   | otherwise = show xs
compressShow xs@[x,y,z,w] 
   | succ2 x y && succ2 y z && succ2 z w = "["++show x++"-"++show w++"]"
   | otherwise = show xs
compressShow xs = show xs

succ2 x y = y==x+1

pass2 incites reflist [] = return ()
pass2 incites reflist s
  | "\\citep{" `isPrefixOf` s = let (cbStr, rest) = span (/='}') s 
                                in do putStr (head incites) 
                                      pass2 (tail incites) reflist (tail rest) 
  | "\\bibliography{" `isPrefixOf` s = let (cbStr, rest) = span (/='}') s 
                                       in do putStr reflist 
                                             pass2 incites reflist (tail rest)
  | otherwise = do putChar (head s)
                   pass2 incites reflist (tail s)

pass1 [] = []
pass1 s | "\\citep{" `isPrefixOf` s = let (cbStr, rest) = span (/='}') s 
                                      in parseCBlock cbStr : pass1 (tail rest)
        | otherwise = pass1 (tail s)

parseCBlock s = let keys = splitBy ',' $ filter (not . (`elem` [' ', '\n'] )) $ takeWhile (/='}') $ tail $ dropWhile (/='{') s
                in CiteBlock True keys s

parseBib bibfileconsts 
   =   case Parsec.parse (Parsec.skipMany Parsec.space >> P.file) "" bibfileconsts of
         Left errMsg -> fail $ (show errMsg)
         Right entries ->
            return entries 

parseAuthors :: String -> [(String, [String])]
parseAuthors = map p . splitByMany " and " where
   p s = let last: rest:_ = splitByMany ", " s
             firsts = splitByOneOf ",. " rest
         in (last, firsts)

grammaticalList :: [String] -> String
grammaticalList [] = []
grammaticalList [s] = s
--grammaticalList [s] = s
grammaticalList ss = intercalate ", " (init ss) ++ " \\& "++last ss

splitByOneOf :: Eq a => [a] -> [a] -> [[a]]
splitByOneOf ps [] = []
splitByOneOf ps s@(c:cs) 
    | c `elem` ps = splitByMany ps (dropWhile (`elem` ps) cs)
    | otherwise = let (hd, tl) = break (`elem` ps) s 
                  in hd : splitByOneOf ps tl
