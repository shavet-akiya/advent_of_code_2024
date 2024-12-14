import Text.Parsec
import Text.Parsec.String

number :: Parser Integer
number = read <$> many1 digit

pairser :: Parser (Integer, Integer)
pairser = do
  _ <- string "mul("
  x <- number
  _ <- char ','
  y <- number
  _ <- char ')'
  return (x,y)


allPairser :: Parser[(Integer, Integer)]
allPairser = many $ try skipMany (noneOf "mul(") *> pairser

getPairs :: String -> Either ParseError [(Integer, Integer)]
getPairs xs = parse allPairser "" xs

muller :: String -> Integer
muller xs = 
  case parse allPairser "" xs of
    Right pairs -> do
      foldl (\r (x,y)-> x*y + r) 0 pairs
    Left _ -> -1

main :: IO ()
main = do
  text <- readFile "input_3_easy.txt"
  print text
  print $ getPairs text
  print $ muller text

