module Main where

import Lib
import System.IO
import Data.List
import qualified Data.Map as M
import qualified Data.IntSet as IS
import Data.List.Split
import qualified Data.Set as S
import Data.Char (isAlpha, toLower)

pathLength = 5

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
  where f amap (documentNum, documentContents) =
            foldl g amap $ wordify documentContents

            -- add a word to a map
          where g amap word = M.insertWith IS.union word (IS.singleton documentNum) amap

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

indexSize :: IIndex -> Int
indexSize (IIndex documents theMap) = M.size theMap

getMap :: IIndex -> (M.Map (Char, Int) IS.IntSet)
getMap (IIndex documents theMap) = theMap

makeAdjList :: [[String]] -> M.Map String (S.Set String)
makeAdjList sll = foldl f M.empty sll
  where f amap sl = M.insert (head sl) (S.fromList $ tail sl) amap

states :: [[String]] -> [String] 
states sll = map head sll


             
-- getPathsInc :: String -> Int -> M.Map String (S.Set String) -> IS.IntSet -> [[String]]
-- getPathsInc start 0 adjlist availDocs = if IS.size (availDocs `intersection` (M.findWithDefault IS.empty start (getMap theindex) )) > 0
--                                         then [[start]], (availDocs `intersection` (M.findWithDefault IS.empty start (getMap theindex) ))
--                                         else [[]], IS.empty
                                             
-- getPathsInc start numRemaining adjlist availDocs = let neighbors = S.elems (adjlist M.! start) :: [String]
--                                           suffixes = concat $ fmap f  neighbors :: [[String]]
--                                           f = (\state -> getPaths state (numRemaining - 1) adjlist) ::  String -> [[String]]
--                                       in fmap (start :) suffixes

             
getPaths :: String -> Int -> M.Map String (S.Set String) -> [[String]]
getPaths start 0 adjlist = [[start]]
getPaths start numRemaining adjlist = let neighbors = S.elems (adjlist M.! start) :: [String]
                                          suffixes = concat $ fmap f  neighbors :: [[String]]
                                          f = (\state -> getPaths state (numRemaining - 1) adjlist) ::  String -> [[String]]
                                      in fmap (start :) suffixes


lowercase = map toLower

filterRepeats  = filter (\x -> length  (group $ sort x) == pathLength)



main :: IO ()
main = do
  -- grab the state adjacency data from https://writeonly.wordpress.com/2009/03/20/adjacency-list-of-states-of-the-united-states-us/
  myfile2 <- openFile "/home/muggli/stateadjsnodc.txt" ReadMode
  statecontents <- hGetContents myfile2
  let mylines2 = lines statecontents
  let mystates = map (splitOn [',']) mylines2
  let adjlist = makeAdjList mystates
  putStrLn ("States: " ++ (show $ states mystates))
  putStrLn ("keys: " ++ (show $ M.keys adjlist))

  myfile <- openFile "/home/muggli/count_1w.txt" ReadMode
  contents <- hGetContents myfile
  let mylines = lines contents
  let mywords = map  (head . words ) mylines
  let validWords = (filter (\y -> (pathLength * 2) == length y)) mywords
  let theindex = buildII $ validWords
  let allSpellable = IS.fromList $ [0..(length validWords)] :: IS.IntSet
  putStrLn ("Map size: "  ++ (show $ indexSize theindex))
  putStrLn ("vocabulary: " ++  (show $ M.keys $ getMap theindex))

  let stillSpellableForLastState pathSoFar stillSpellable = let wordified = wordify pathSoFar
                                                                partialStillSpellable = (M.findWithDefault IS.empty (last (init wordified)) (getMap theindex))  `IS.intersection` stillSpellable
                                                            in (M.findWithDefault IS.empty  (last wordified) (getMap theindex))  `IS.intersection` partialStillSpellable

           
  let wordsForState adjlist state = fmap lowercase $ fmap concat $ filterRepeats $ getPathsInc state (pathLength - 1) (stillSpellableForLastState state allSpellable) state
          where
            -- given a state, a length, and the set of words still spellable so far, return the state sequece suffixes
            getPathsInc :: String -> Int -> IS.IntSet -> String ->  [[String]]
            getPathsInc start numRemaining stillSpellable pathSoFar
                        | stillSpellable == IS.empty = []
                        | numRemaining == 0 = [[start]]
                        | otherwise =  let neighbors = S.elems (adjlist M.! start) :: [String]
                                           suffixes = concat $ fmap f  neighbors :: [[String]]
                                           f = (\state -> getPathsInc state (numRemaining - 1) (stillSpellableForLastState (pathSoFar ++ state) stillSpellable) (pathSoFar ++ state)) ::  String -> [[String]]
                                       in fmap (start :) suffixes
            
  let allwords = concat $ fmap (wordsForState adjlist) $ states mystates
  putStrLn ("all words number: " ++ (show $ length $ allwords))

  

  let getAnagrams = (\targetword -> queryII (wordify targetword) theindex)
  let positivePaths = (filter (\z -> (length $ getAnagrams z) > 0) allwords)
  let anagramLists = (fmap getAnagrams  positivePaths) :: [[String]]
  let allAnagrams = nub $ sort $  concat anagramLists :: [String]
  putStrLn ("Unique anagrams: " ++  (show  allAnagrams))
  let allstatewords = zip positivePaths (fmap getAnagrams  positivePaths)
  putStrLn ("num stanagrams: " ++ (show $ length allstatewords))
  putStrLn ("stanagrams: " ++ (show $  allstatewords))

  hClose myfile2           
  hClose myfile
