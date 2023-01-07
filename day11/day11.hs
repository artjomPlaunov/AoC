import Data.Map (Map)
import qualified Data.Map as Map

data Monk = Monk {
  monkID :: Int,
  items :: [Int],
  op :: Int -> Int,
  test :: Int -> Bool,
  testTrue :: Int,
  testFalse :: Int
  }

instance Show Monk where
  show (Monk monkID items op test testTrue testFalse) =
    show monkID <> "\n" <> show items <> "\n" <> show testTrue <> "\n" <>
    show testFalse <> "\n"

-- Check if n is divisible by m.
-- m is passed first for currying purposes. 
isDiv :: Int -> Int -> Bool
isDiv m n
  | n `mod` m == 0 = True
  | otherwise = False

-- Process n rounds
goRounds ::Map.Map Int Monk ->
           Map.Map Int Int ->
           [(Int, Monk)] ->
           Int ->
           (Map.Map Int Monk, Map.Map Int Int) 
goRounds monkMap countMap monks 0 = (monkMap, countMap)
goRounds monkMap countMap monks n =
  let (monkMap', countMap') = goRound monkMap countMap monks in
  let monks' = (Map.toAscList monkMap') in
    goRounds monkMap' countMap' monks' (n-1)

-- Process one round
goRound :: Map.Map Int Monk ->
           Map.Map Int Int ->
           [(Int, Monk)] ->
           (Map.Map Int Monk, Map.Map Int Int)
goRound monkMap countMap [] = (monkMap, countMap)
goRound monkMap countMap (m:ms) =
  -- Update count map
  let monk = Map.findWithDefault (snd m) (fst m) monkMap  in
  let countMap' = Map.adjust (+ (length $ items monk)) (fst m) countMap in
  let monkMap' = goMonk monk monkMap (items monk) in
    goRound monkMap' countMap' ms

-- Process list of items at current monk.
goMonk :: Monk ->
          Map.Map Int Monk ->
          [Int] ->
          Map.Map Int Monk
goMonk monk monkMap [] =
  -- Once we're done processing, make sure to empty out items list for monk.
  let adjustFunc m = m { items = [] } in
  Map.adjust adjustFunc (monkID monk) monkMap
goMonk monk monkMap (i:is) =
  let worry = ((op monk) i)
      adjustFunc m = m { items = (items m)++[worry `mod` 9699690]} 
  in 
    if (test monk) worry
    then goMonk monk (Map.adjust adjustFunc (testTrue monk) monkMap) is
    else goMonk monk (Map.adjust adjustFunc (testFalse monk) monkMap) is
      
--mod1=96577
--mod2=9699690

--------------------------------------------------------------------------------
--Hardcoded input for main program.
--------------------------------------------------------------------------------
main :: IO ()
main =
  do
    let monk0 = Monk {monkID = 0, items = [79, 98], op = (*19),
                      test = (isDiv 23),  testTrue = 2, testFalse = 3}
    let monk1 = Monk {monkID = 1, items = [54, 65, 75, 74], op = (+6),
                      test = (isDiv 19), testTrue = 2, testFalse = 0}
    let monk2 = Monk {monkID = 2, items = [79, 60, 97], op = (\x -> x * x),
                      test = (isDiv 13), testTrue = 1, testFalse = 3}
    let monk3 = Monk {monkID = 3, items = [74], op = (+3),
                      test = (isDiv 17), testTrue = 0, testFalse = 1}
    let monks = [monk0, monk1, monk2, monk3]
    let order = [0, 1, 2, 3]
    let monk0 = Monk {monkID = 0, items = [52, 60, 85, 69, 75, 75], op = (*17),
                      test = (isDiv 13), testTrue = 6, testFalse = 7}
    let monk1 = Monk {monkID = 1, items = [96, 82, 61, 99, 82, 84, 85],
                      op = (+8),
                      test = (isDiv 7), testTrue = 0, testFalse = 7}
    let monk2 = Monk {monkID = 2, items = [95, 79], op = (+6),
                      test = (isDiv 19),  testTrue = 5, testFalse = 3}
    let monk3 = Monk {monkID = 3, items = [88, 50, 82, 65, 77], op = (*19),
                      test = (isDiv 2),  testTrue = 4, testFalse = 1}
    let monk4 = Monk {monkID = 4, items = [66, 90, 59, 90, 87, 63, 53, 88],
                      op = (+7),
                      test = (isDiv 5), testTrue = 1, testFalse = 0}
    let monk5 = Monk {monkID = 5, items = [92, 75, 62], op = (\x -> x * x),
                      test = (isDiv 3),  testTrue = 3, testFalse = 4}
    let monk6 = Monk {monkID = 6, items = [94, 86, 76, 67], op = (+1),
                      test = (isDiv 11),  testTrue = 5, testFalse = 2}
    let monk7 = Monk {monkID = 7, items = [57], op = (+2),
                      test = (isDiv 17),  testTrue = 6, testFalse = 2}
    let monks = [monk0, monk1, monk2, monk3, monk4, monk5, monk6, monk7]
    let order = [0, 1, 2, 3, 4, 5, 6, 7]
    -- map monkID -> monk
    let monkMap = Map.fromList (zip order monks)
    let countMap = Map.fromList (zip order [0,0,0,0,0,0,0,0])
    let (monkMap', countMap') =
          goRounds monkMap countMap (Map.toAscList monkMap) 10000
    putStrLn $ show countMap'
    
    
    

