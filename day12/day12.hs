{-- Day 12: Hill Climbing Algorithm
    Artjom Plaunov --}

module Main where

import Lib
import Data.Map (Map)
import qualified Data.Map as M
import System.IO
import Data.Char
import Data.Maybe
import Data.List
import Debug.Trace

main :: IO ()
main =
  do
    input <- readFile "day12.txt"
    let input' = lines input
    let (vertices, start, end) = parseVertices input' 
    let x = length input'
    let y = length $ head input'
    let inBounds' = inBounds x y
    let edges = makeEdges vertices inBounds'
    let shortPath= dijkstra edges end
    let vs = M.toList vertices
    let aLocs = map fst (filter (\v -> (snd v) `elem` [97, 83]) vs)
    let a's = filter (\v -> (fst v) `elem` aLocs) (M.toList shortPath)
    putStrLn $ show a's
    
  
getMinQ :: [(Int, Int)] ->
           M.Map (Int, Int) Int ->
           Int ->
           (Int, Int) ->
           (Int, Int)
getMinQ [] dist curVal curMin = curMin
getMinQ ((x,y):xs) dist curVal curMin =
  let next = (fromMaybe (maxBound :: Int) (M.lookup (x,y) dist))
  in
    if next <= curVal
    then
      getMinQ xs dist next (x,y)
    else getMinQ xs dist curVal curMin

relaxEdge :: (Int, Int) ->
             (Int, Int) ->
             M.Map (Int, Int) Int ->
             M.Map (Int, Int) Int
relaxEdge u v dist =
  let alt = (fromMaybe (maxBound :: Int) (M.lookup u dist)) + 1
      distv = fromMaybe (maxBound :: Int) (M.lookup v dist)
  in
    if alt < distv
    then M.insert v alt dist
    else dist
  
processQ :: [(Int, Int)] ->
            M.Map (Int, Int) Int ->
            M.Map (Int, Int) [(Int, Int)] ->
            M.Map (Int, Int) Int
processQ [] dist edges = dist
processQ q dist edges =
  let u = getMinQ q dist (maxBound :: Int) (-1, -1)
      vs = fromMaybe [] (M.lookup u edges)
      vs' = filter (`elem` q) vs
      q' = delete u q
  in
    processQ q' (foldr (relaxEdge u) dist vs') edges
          

dijkstra :: M.Map (Int, Int) [(Int, Int)] ->
            (Int, Int) ->
            M.Map (Int, Int) Int
dijkstra edges source =
  let -- Set all distances to infinity, and the source distance to 0.
      dist' = M.map (\_ -> (maxBound :: Int)) edges
      dist = M.insert source 0 dist'
      q = map fst (M.toList edges)
  in
    processQ q dist edges
    
  
  
inBounds :: Int -> Int -> (Int, Int) -> Bool
inBounds x y (i, j) =
  (0 <= i) && (i <= x) && (0 <= j) && (j <= y) 

filterEdge :: Int ->
              M.Map (Int, Int) Int ->
              (Int, Int) ->
              Bool
filterEdge n vertices vertex =
  let val = fromMaybe 0 (M.lookup vertex vertices) in n <= val + 1

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
