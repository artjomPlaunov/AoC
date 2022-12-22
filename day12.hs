{-- Day 12: Hill Climbing Algorithm
    Artjom Plaunov --}

module Main where

import Lib
import Data.Map (Map)
import qualified Data.Map as M
import System.IO
import Data.Char
import Data.Maybe

main :: IO ()
main =
  do
    input <- readFile "day12Test.txt"
    let input' = lines input
    let (vertices, start, end) = parseVertices input' 
    putStrLn $ show start
    putStrLn $ show end
    putStrLn $ show vertices
    let x = length input'
    let y = length $ head input'
    let inBounds' = inBounds x y
    let edges = makeEdges vertices inBounds'
    let shortestPaths = dijkstra edges start
    let min = fromMaybe 0 (M.lookup end shortestPaths)
    putStrLn $ show min
    putStrLn $ show shortestPaths

getMin :: [(Int, Int)] ->
          


dijkstra :: M.Map (Int, Int) [(Int, Int)] ->
            (Int, Int) ->
            M.Map (Int, Int) Int
dijkstra edges source =
  let -- Set all distances to infinity, and the source distance to 0.
      dist' = M.map (\_ -> (maxBound :: Int)) edges
      dist = M.insert source 0 dist'
      q = map fst (M.toList edges)
  in
    
  
  
inBounds :: Int -> Int -> (Int, Int) -> Bool
inBounds x y (i, j) =
  (0 <= i) && (i <= x) && (0 <= j) && (j <= y) 

filterEdge :: Int ->
              M.Map (Int, Int) Int ->
              (Int, Int) ->
              Bool
filterEdge n vertices vertex =
  let val = fromMaybe 0 (M.lookup vertex vertices) in n <= val+1

foldEdges :: M.Map (Int, Int) Int ->
             ((Int, Int) -> Bool) ->
             (Int, Int) ->
             M.Map (Int, Int) [(Int, Int)] ->
             M.Map (Int, Int) [(Int, Int)]
foldEdges vertices inBounds (i, j) edges =
  let offsets = filter inBounds [(i, j-1), (i-1, j), (i, j+1), (i+1, j)]
      curVal = fromMaybe 0 (M.lookup (i, j) vertices)
      offsets' = filter (filterEdge curVal vertices) offsets
  in
    M.insert (i, j) offsets' edges
    

makeEdges :: M.Map (Int, Int) Int ->
             ((Int, Int) -> Bool) ->
             M.Map (Int, Int) [(Int, Int)]
makeEdges vertices inBounds =
  foldr (foldEdges vertices inBounds) M.empty (fmap fst (M.toList vertices))
  
parseVertices :: [[Char]] -> (M.Map (Int, Int) Int, (Int, Int), (Int, Int))
parseVertices lst = parseRows lst 0 M.empty (-1,-1) (-1,-1)

parseRows :: [[Char]] ->
             Int ->
             M.Map (Int, Int) Int ->
             (Int, Int) ->
             (Int, Int) ->
             (M.Map (Int, Int) Int, (Int, Int), (Int, Int))
parseRows [] rowNum vertices start end = (vertices, start, end)
parseRows (r:rs) rowNum vertices start end =
  let (vertices', start', end') = parseRow r rowNum 0 vertices start end in
    parseRows rs (rowNum+1) vertices' start' end'

parseRow :: [Char] ->
            Int ->
            Int ->
            M.Map (Int, Int) Int ->
            (Int, Int) ->
            (Int, Int) ->
            (M.Map (Int, Int) Int, (Int, Int), (Int, Int)) 
parseRow [] x y vertices start end = (vertices, start, end)
parseRow (c:cs) x y vertices start end =
  let (start', end') = getStartEnd x y start end c in
    if c == 'S' || c == 'a'
    then
      parseRow cs x (y+1) (M.insert (x,y) (ord 'a') vertices) start' end'
    else 
      if c == 'E' || c == 'z'
      then
        parseRow cs x (y+1) (M.insert (x,y) (ord 'z') vertices) start' end'
      else
        parseRow cs x (y+1) (M.insert (x,y) (ord c) vertices) start' end'
      
getStartEnd :: Int ->
               Int ->
               (Int, Int) ->
               (Int, Int) ->
               Char
               -> ((Int, Int), (Int, Int))
getStartEnd x y start end c
  | c == 'S' = ((x,y), end)
  | c == 'E' = (start, (x,y))
  | otherwise = (start, end)
