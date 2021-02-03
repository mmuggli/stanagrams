module Main where

import Lib
import System.IO
import Data.List
import qualified Data.Map as M
import qualified Data.IntSet as IS
import Data.List.Split
import qualified Data.Set as S
import Data.Char (isAlpha, toLower)


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


lowercase = map toLower

filter_repeats  = filter (\x -> length  (group $ sort x) == 4)

words_for_state adjlist state = fmap lowercase $ fmap concat $ filter_repeats $ get_paths state 3 adjlist

main :: IO ()
main = do
  -- grab the state adjacency data from https://writeonly.wordpress.com/2009/03/20/adjacency-list-of-states-of-the-united-states-us/
  myfile2 <- openFile "/home/muggli/stateadjsnodc.txt" ReadMode
  statecontents <- hGetContents myfile2
  let mylines2 = lines statecontents
  let mystates = map (splitOn [',']) mylines2
  let adjlist = make_adjlist mystates
  putStrLn ("States: " ++ (show $ states mystates))
  putStrLn ("keys: " ++ (show $ M.keys adjlist))
  putStrLn ("CO paths len 4: " ++ (show $ words_for_state  adjlist "CO"))
  let allwords = concat $ fmap (words_for_state adjlist) $ states mystates
  putStrLn ("all words number: " ++ (show $ length $ allwords))
  hClose myfile2
  
  myfile <- openFile "/home/muggli/count_1w.txt" ReadMode
  contents <- hGetContents myfile
  let mylines = lines contents
  let mywords = map  (head . words ) mylines
  let theindex = buildII $ (filter (\y -> 8 == length y)) mywords
  putStrLn ("Map size: "  ++ (show $ index_size theindex))
  putStrLn ("vocabulary: " ++  (show $ M.keys $ get_map theindex))
--    putStrLn ("query results for 'of': " ++ (show $ queryII (wordify "of") theindex))
  let get_anagrams = (\targetword -> queryII (wordify targetword) theindex)
  let positive_paths = (filter (\z -> (length $ get_anagrams z) > 0) allwords)
  let anagram_lists = (fmap get_anagrams  positive_paths) :: [[String]]
  let all_anagrams = nub $ sort $  concat anagram_lists :: [String]
  putStrLn ("Unique anagrams: " ++  (show  all_anagrams))
  let allstatewords = zip positive_paths (fmap get_anagrams  positive_paths)
  putStrLn ("num stanagrams: " ++ (show $ length allstatewords))
  putStrLn ("stanagrams: " ++ (show $  allstatewords))
  --putStrLn $ show $ get_map theindex
  -- putStrLn $ show $ wordify $ head mywords -- 
  hClose myfile
