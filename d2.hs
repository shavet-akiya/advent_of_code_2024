import Data.Maybe

findSafe :: [Int] -> Maybe Int
findSafe [] = Nothing
findSafe xs
 | all (>0) xs || all (<0) xs = Just $ 1
 | otherwise = Nothing

diffFinder :: [Int] -> [Int]
diffFinder [] = []
diffFinder [n] = []
diffFinder (n1 : n2 : ns)
 | abs (n1 - n2) <= 3 && n1 /= n2 =  (n1 - n2) : (diffFinder (n2 : ns))
 | otherwise = [0]

findSafe2 :: [Int] -> Maybe Int
findSafe2 [] = Nothing
findSafe2 xs
 | 0 elem xs = Nothing
 | all (>0) xs || all (<0) xs = Just $ 1
 | otherwise = Nothing
 where
  onlyPos = filter (>0) xs
  onlyNeg = filter (<0) xs

diffFinder2 :: [Int] -> Int -> Int -> [Int]
diffFinder2 (x:y:z) b 0 = diffFinder2 (x:y:z) b (x - y)
diffFinder2 [] _ _ = [0]
diffFinder2 [n] _ _ = []
diffFinder2 (n1 : n2 : ns) x s
 | sameChecker s diff && abs diff <= 3 && diff /= 0 =  diff : (diffFinder2 (n2 : ns) x diff)
 | x == 0 = diffFinder2 (n1 : ns) 1
 | otherwise = [0]
 where
  diff = n1 - n2
  sameChecker = (\x y -> (x > 0 && y > 0) || (x < 0 && y < 0)) 
 
main :: IO ()
main = do
  text <- readFile "input_2_easy.txt"
  let allLines = lines text
  let goodRows = fmap (fmap read . words) allLines
  let l1 = fmap (\x -> diffFinder2 x 0 0) goodRows
  let l2 = fmap findSafe l1
  print goodRows
  print l1
  print l2
  print "-----"
  let finalSafe = mapMaybe (findSafe . (\x -> diffFinder2 x 0 0)) goodRows
  --print finalSafe
  print $ length finalSafe
  -- let decOnly = mapMaybe (\x -> ) goodRows
  print "done"
