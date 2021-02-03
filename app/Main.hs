module Main where

import Lib
import System.IO
import Data.List
import qualified Data.Map as M
import qualified Data.IntSet as S

-- copied from https://rosettacode.org/wiki/Inverted_index#Haskell on Feb 3, 2021
-- we swap (Char, Int) for String
-- and String for FilePath
data IIndex = IIndex
    [String]              -- documents in the index
    (M.Map (Char, Int) S.IntSet) -- Maps "word" (char, occurance) to indices of the list
  deriving Show


buildII :: [String] -> IIndex
buildII documents =
     IIndex documents  (foldl f M.empty  (zip [0..]  documents))

    -- add a document to a map
    -- f amap documentnum documentcontents
  where f amap (document_num, document_contents) =
            foldl g amap $ wordify document_contents

            -- add a word to a map
          where g amap word = M.insertWith S.union word (S.singleton document_num) amap




enumerate :: String -> [(Char, Int)]
enumerate s = zip s [1..]

wordify :: String -> [(Char, Int)]
wordify s = let groups = group $ sort s in
              concat $ map enumerate groups

index_size :: IIndex -> Int
index_size (IIndex documents the_map) = M.size the_map

get_map :: IIndex -> (M.Map (Char, Int) S.IntSet)
get_map (IIndex documents the_map) = the_map

main :: IO ()
main = do
  myfile <- openFile "/home/muggli/count_1w.txt" ReadMode
  contents <- hGetContents myfile
  let mylines = lines contents
  let mywords = map  (head . words ) mylines
  let theindex = buildII mywords
  putStrLn ("Map size: "  ++ (show $ index_size theindex))
  putStrLn ("vocabulary: " ++  (show $ M.keys $ get_map theindex))
  --putStrLn $ show $ get_map theindex
  -- putStrLn $ show $ wordify $ head mywords -- 
  hClose myfile
