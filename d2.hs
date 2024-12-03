import Data.Maybe

findSafe :: [Int] -> Maybe Int
findSafe [] = Nothing
findSafe xs
 | all (>0) xs || all (<0) xs = Just $ sum xs
 | otherwise = Nothing

-- findSafe (x:y:xs)

diffFinder :: [Int] -> [Int]
diffFinder [] d = d
diffFinder [n] d = d
diffFinder (n1 : n2 : ns) d = (n1 - n2) : (diffFinder (n2 : ns))

main :: IO ()
main = do
  text <- readFile "input_2_easy.txt"
  let allLines = lines text
  let goodRows = fmap words allLines
  let finalSafe = mapMaybe (findSafe . diffFinder) goodRows
  print $ length finalSafe
  -- let decOnly = mapMaybe (\x -> ) goodRows
  print "done"
