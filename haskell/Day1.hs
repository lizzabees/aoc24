module Main where

import Text.Parsec

type Parser a = Parsec String () a

rows :: Parser ([Int], [Int])
rows = do
  rs <- sepEndBy1 row newline
  let left = foldr (insertBy compare . fst) [] rs
  let rght = foldr (insertBy compare . snd) [] rs
  return (left, rght)

  where
    int :: Parser Int
    int = read <$> many1 digit

    row :: Parser (Int, Int)
    row = (,) <$> int <*> (skipMany1 space *> int)


part1 :: ([Int], [Int]) -> Int
part1 (ls, rs) = sum $ zipWith (\x y -> abs $ x - y) ls rs

part2 :: ([Int], [Int]) -> Int
part2 (ls, rs) = sum $ map (\x -> x * (length $ elemIndices x rs)) ls

main :: IO ()
main = do
  path <- head <$> getArgs
  text <- readFile path
  let input = either (error . show) id $ runParser rows () path text
  putStrLn $ mconcat ["part1: ", show $ part1 input]
  putStrLn $ mconcat ["part2: ", show $ part2 input]
