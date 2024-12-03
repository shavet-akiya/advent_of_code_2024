import Data.List

distanceFinder :: [Int] -> [Int] -> Int
distanceFinder [] _ = 0
distanceFinder _ [] = 0
distanceFinder (l1 : r1) (l2 : r2) = abs (l1 - l2) + distanceFinder r1 r2

repeatFinder :: [Int] -> [(Int, Int)] -> Int
repeatFinder [] _ = 0
repeatFinder _ [] = 0
repeatFinder (a : as) ((b1, b2) : bs)
  | a > b1 = repeatFinder (a : as) bs
  | a < b1 = repeatFinder as ((b1, b2) : bs)
  | a == b1 = a * b2 + repeatFinder as ((b1, b2) : bs)
main :: IO ()
main = do
  text <- readFile "input_1.txt"
  let line = lines text
  let split = fmap (fmap read . words) line
  let (l1, l2) = (fmap head split, fmap last split)
  let (list1, list2) = (sort l1, sort l2)
  print $ distanceFinder list1 list2
  let sl1 = foldl (\x y -> if y `elem` x then x else x ++ [y]) [] list1
  let sl2 = (fmap (\x -> (head x, length x)) . group) list2
  print $ repeatFinder list1 sl2
  print "done"
