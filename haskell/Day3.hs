module Main where

import Prelude hiding ((<|>), many, try)
import Text.Parsec

type Parser a = Parsec String () a

data Cmd = Mul Int Int
  deriving (Eq, Show)

int :: Parser Int
int = read <$> some digit

cmds :: Bool -> Parser [Cmd]
cmds stmts = go True []
  where 
    go :: Bool -> [Cmd] -> Parser [Cmd]
    go doing cs = choice . map try $
      [ do { eof; return $           reverse  cs }
      , do { _ <- string "do()";     go True  cs }
      , do { _ <- string "don't()";  go False cs }
      , do 
        c <- cmd
        case (stmts, doing) of
          (False,    _) -> go doing (c:cs)
          (True,  True) -> go True  (c:cs)
          (True, False) -> go False    cs
      , do { _ <- anyChar; go doing cs  }
      ]

    cmd :: Parser Cmd
    cmd = do
      lhs <- string "mul(" *> int
      rhs <- char ',' *> int <* char ')'
      return $ Mul lhs rhs

results :: [Cmd] -> Int
results = foldr (\(Mul l r) acc -> acc + l * r) 0

main :: IO ()
main = do
  path <- head <$> getArgs
  text <- readFile path
  let input = \p -> either (error . show) id $ runParser p () path text
  putStrLn $ mconcat ["part1: ", show . results . input $ cmds False]
  putStrLn $ mconcat ["part2: ", show . results . input $ cmds True]
