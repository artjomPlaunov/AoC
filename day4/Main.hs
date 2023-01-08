import Data.List.Split

type Pair = ((Integer, Integer), (Integer, Integer))

main :: IO ()
main = do
  inputLines <- (fmap lines . readFile) "day4.txt"
  let pairs = map parseInput inputLines
  let count = length $ filter isSubset pairs
  let count = length $ filter isOverlap pairs
  print count

isSubset :: Pair -> Bool
isSubset ((n1, n2), (n3, n4)) =
  ((n1 >= n3) && (n2 <= n4)) || ((n3 >= n1) && (n4 <= n2))

isOverlap :: Pair -> Bool
isOverlap ((n1, n2), (n3, n4)) =
  ((n1 >= n3) && (n1 <= n4)) || ((n3 >= n1) && (n3 <= n2))


parseInput :: String -> Pair
parseInput s =
  let pair = splitOn "," s
      pair1 = head pair
      pair2 = head $ tail pair
      nums1 = splitOn "-" pair1
      nums2 = splitOn "-" pair2
      n1 = read (head nums1) :: Integer
      n2 = read (head $ tail nums1) :: Integer
      n3 = read (head nums2) :: Integer
      n4 = read (head $ tail nums2) :: Integer
  in
    ((n1, n2), (n3, n4))


