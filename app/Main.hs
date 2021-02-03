module Main where

import Lib
import System.IO
import Data.List

enumerate :: String -> [(Char, Int)]
enumerate s = zip s [1..]

wordify :: String -> [(Char, Int)]
wordify s = let groups = group $ sort s in
              concat $ map enumerate groups

                

main :: IO ()
main = do
  myfile <- openFile "/home/muggli/count_1w.txt" ReadMode
  contents <- hGetContents myfile
  let mylines = lines contents
  let mywords = map  (head . words ) mylines
  putStrLn $show $ wordify $ head mywords
  hClose myfile
