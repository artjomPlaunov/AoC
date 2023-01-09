module Main where

import Data.List.Split
import qualified Data.Map as M
import Debug.Trace

main :: IO ()
main = do
  --inputLines <- (fmap lines . readFile) "day6.txt"
  input <- readFile "day6.txt"
  let stream = reverse $ tail $ reverse input
  let marker = findMarker stream 14
  print marker

findMarker :: [Char] -> Int -> Int
findMarker s n =
  if length s < 14 then n else
    if isUnique (take 14 s) then n else findMarker (tail s) (n+1)

isMarker :: [Char] -> Bool
isMarker [c1,c2,c3,c4] =
  if (c1 /= c2) && (c1 /= c3) && (c1 /= c4) &&
     (c2 /= c3) && (c2 /= c4) && (c3 /= c4)
  then True
  else False
isMarker _ = False

isUnique :: [Char] -> Bool
isUnique l = (length l) == (length $ unique l)

unique [] = []
unique (x:xs) = x:unique (filter ((/=) x) xs)







