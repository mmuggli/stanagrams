module Main where

import Lib
import System.IO
import Data.List
import qualified Data.Map as M
import qualified Data.IntSet as S
import Data.List.Split

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

queryII :: [(Char, Int)] -> IIndex -> [String]
queryII q (IIndex documents m) =
    map (documents !!) $ S.toList $ intersections $
    map (\word -> M.findWithDefault S.empty word m) q
 
intersections [] = S.empty
intersections xs = foldl1 S.intersection xs


-- my stuff (i.e. end of code from rosettacode)

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
  -- grab the state adjacency data from https://writeonly.wordpress.com/2009/03/20/adjacency-list-of-states-of-the-united-states-us/
  myfile2 <- openFile "/home/muggli/stateadj.txt" ReadMode
  statecontents <- hGetContents myfile2
  let mylines2 = lines statecontents
  let mystates = map (splitOn [',']) mylines2
  hClose myfile2
  -- myfile <- openFile "/home/muggli/count_1w7.txt" ReadMode
  -- contents <- hGetContents myfile
  -- let mylines = lines contents
  -- let mywords = map  (head . words ) mylines
  -- let theindex = buildII mywords
  -- putStrLn ("Map size: "  ++ (show $ index_size theindex))
  -- putStrLn ("vocabulary: " ++  (show $ M.keys $ get_map theindex))
  -- putStrLn ("query results for 'of': " ++ (show $ queryII (wordify "of") theindex))
  -- --putStrLn $ show $ get_map theindex
  -- -- putStrLn $ show $ wordify $ head mywords -- 
  -- hClose myfile
