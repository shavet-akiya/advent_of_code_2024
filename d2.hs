import Data.Maybe

findSafe :: [[Int]] -> Int
findSafe [] = 0

-- findSafe (x:y:xs)

diffFinder :: [Int] -> [Int] -> Maybe Int
diffFinder [] d = Just $ length d
diffFinder [n] d = Just $ length d
diffFinder (n1 : n2 : ns) d = Just 1 + (n1 - n2) : d

main :: IO ()
main = do
  text <- readFile "input_2_easy.txt"
  let allLines = lines text
  let goodRows = fmap words allLines
  -- let decOnly = mapMaybe (\x -> ) goodRows
  print "done"
