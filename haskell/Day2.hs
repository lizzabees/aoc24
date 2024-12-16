module Main where

import Data.List (any)
import Text.Parsec
import GHC.Arr (safeRangeSize)

type Parser a = Parsec String () a

rows :: Parser [[Int]]
rows = sepEndBy1 row newline
  where 
    row :: Parser [Int]
    row = sepBy1 int (char ' ')

    int :: Parser Int
    int = read <$> many1 digit

safe :: [Int] -> Bool
safe xs =
  let
    dxs = zipWith (\x y -> abs $ x - y) xs (tail xs)
    cxs = zipWith compare xs (tail xs)
    asc = not $ any (== LT) cxs
    dsc = not $ any (== GT) cxs
    dir = asc || dsc
    mag = not $ any (\x -> x < 1 || x > 3) dxs
  in dir && mag

-- take a list and return every version of a list sans
-- one element
holes :: [Int] -> [[Int]]
holes = go [] []
  where go :: [[Int]] -> [Int] -> [Int] -> [[Int]] 
        go _   _   []    = error "invalid input" -- not gonna generalize
        go []  _  (r:rs) = go [rs] [r] rs        -- first elem
        go acc ls (_:[]) = (reverse ls):acc      -- last elem
        go acc ls (r:rs) =
          let acc' = ((reverse ls) ++ rs):acc
              ls'  = r:ls
           in go acc' ls' rs
        

part1 :: [[Int]] -> Int
part1 = length . filter safe

part2 :: [[Int]] -> Int
part2 = length . filter (any safe . holes)

main :: IO ()
main = do
  path <- head <$> getArgs
  text <- readFile path
  let input = either (error . show) id $ runParser rows () path text
  putStrLn $ mconcat ["part1: ", show $ part1 input]
  putStrLn $ mconcat ["part2: ", show $ part2 input]
