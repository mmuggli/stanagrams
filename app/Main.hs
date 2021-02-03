module Main where

import Lib
import System.IO
import Data.List
import qualified Data.Map as M
import qualified Data.IntSet as IS
import Data.List.Split
import qualified Data.Set as S

-- copied from https://rosettacode.org/wiki/Inverted_index#Haskell on Feb 3, 2021
-- we swap (Char, Int) for String
-- and String for FilePath
data IIndex = IIndex
    [String]              -- documents in the index
    (M.Map (Char, Int) IS.IntSet) -- Maps "word" (char, occurance) to indices of the list
  deriving Show


buildII :: [String] -> IIndex
buildII documents =
     IIndex documents  (foldl f M.empty  (zip [0..]  documents))

    -- add a document to a map
    -- f amap documentnum documentcontents
  where f amap (document_num, document_contents) =
            foldl g amap $ wordify document_contents

            -- add a word to a map
          where g amap word = M.insertWith IS.union word (IS.singleton document_num) amap

queryII :: [(Char, Int)] -> IIndex -> [String]
queryII q (IIndex documents m) =
    map (documents !!) $ IS.toList $ intersections $
    map (\word -> M.findWithDefault IS.empty word m) q
 
intersections [] = IS.empty
intersections xs = foldl1 IS.intersection xs


-- my stuff (i.e. end of code from rosettacode)

enumerate :: String -> [(Char, Int)]
enumerate s = zip s [1..]

wordify :: String -> [(Char, Int)]
wordify s = let groups = group $ sort s in
              concat $ map enumerate groups

index_size :: IIndex -> Int
index_size (IIndex documents the_map) = M.size the_map

get_map :: IIndex -> (M.Map (Char, Int) IS.IntSet)
get_map (IIndex documents the_map) = the_map

make_adjlist :: [[String]] -> M.Map String (S.Set String)
make_adjlist sll = foldl f M.empty sll
  where f amap sl = M.insert (head sl) (S.fromList $ tail sl) amap

states :: [[String]] -> [String] 
states sll = map head sll

get_paths :: String -> Int -> M.Map String (S.Set String) -> [[String]]
get_paths start 0 adjlist = [[start]]
get_paths start num_remaining adjlist = let neighbors = S.elems (adjlist M.! start) :: [String]
                                            suffixes = concat $ fmap f  neighbors :: [[String]]
                                            f = (\state -> get_paths state (num_remaining - 1) adjlist) ::  String -> [[String]]
                                        in fmap (start :) suffixes

  
main :: IO ()
main = do
  -- grab the state adjacency data from https://writeonly.wordpress.com/2009/03/20/adjacency-list-of-states-of-the-united-states-us/
  myfile2 <- openFile "/home/muggli/stateadjs.txt" ReadMode
  statecontents <- hGetContents myfile2
  let mylines2 = lines statecontents
  let mystates = map (splitOn [',']) mylines2
  let adjlist = make_adjlist mystates
  putStrLn ("States: " ++ (show $ states mystates))
  putStrLn ("keys: " ++ (show $ M.keys adjlist))
  putStrLn ("CO paths len 4: " ++ (show $ get_paths "CO" 3 adjlist))
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
