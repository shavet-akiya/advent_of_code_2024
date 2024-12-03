import Data.Maybe

findSafe :: [Int] -> Maybe Int
findSafe [] = Nothing
findSafe xs
 | all (>0) xs || all (<0) xs = Just $ sum xs
 | otherwise = Nothing

-- findSafe (x:y:xs)

diffFinder :: [Int] -> [Int]
diffFinder [] = []
diffFinder [n] = [n]
diffFinder (n1 : n2 : ns) = (n1 - n2) : (diffFinder (n2 : ns))

main :: IO ()
main = do
  text <- readFile "input_2_easy.txt"
  let allLines = lines text
  let goodRows = fmap (fmap read . words) allLines
  let l1 = fmap diffFinder goodRows
  let l2 = fmap findSafe l1
  print l1
  print l2
  print "-----"
  let finalSafe = mapMaybe (findSafe . diffFinder) goodRows
  print $ length finalSafe
  -- let decOnly = mapMaybe (\x -> ) goodRows
  print "done"
