module Main where

import Data.List.Split
import qualified Data.Map as M
import Debug.Trace

main :: IO ()
main = do
  inputLines <- (fmap lines . readFile) "day5.txt"
  let moves = map parseInput inputLines
  let crates = M.fromList [
        (1, ["S", "P", "H", "V", "F", "G"]),
        (2, ["M", "Z", "D", "V", "B", "F", "J", "G"]),
        (3, ["N", "J", "L", "M", "G"]),
        (4, ["P", "W", "D", "V", "Z", "G", "N"]),
        (5, ["B", "C", "R", "V"]),
        (6, ["Z", "L", "W", "P", "M", "S", "R", "V"]),
        (7, ["P", "H", "T"]),
        (8, ["V", "Z", "H", "C", "N", "S", "R", "Q"]),
        (9, ["J", "Q", "V", "P", "G", "L", "F"])]
  let cratesTest = M.fromList [
        (1, ["N", "Z"]),
        (2, ["D", "C", "M"]),
        (3, ["P"])]
  let res1 = foldr moveCrates crates (reverse moves)
  let res = map snd (M.assocs (foldr moveCrates crates moves))
  print res1
  

moveCrates :: (Int, Int, Int) ->
              M.Map Int [String] ->
              M.Map Int [String]
moveCrates (n, src, dest) crates =
  let srcCrates = M.findWithDefault [] src crates in
  let destCrates = M.findWithDefault [] dest crates in
  let cratesMove = (take n srcCrates) in
  let m1 = M.insert src (drop n srcCrates) crates in
    M.insert dest (cratesMove ++ destCrates) m1
  
parseInput :: String -> (Int, Int, Int)
parseInput s =
  let s' = splitOn " " s in
    ((read (s' !! 1) :: Int),
     (read (s' !! 3) :: Int),
     (read (s' !! 5) :: Int))








