{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Transaction.CSV
       ( ParseError, Row, Doc, HeaderP(..), (!), csv, parseCSV, csvHeader, csvContents,
         getCells ) where

import Text.ParserCombinators.Parsec

type Cell = String
newtype Row = Row [Cell]
data Doc = Doc {
  csvHeader :: Maybe Row,
  csvContents :: [Row]
  }

getCells :: Row -> [Cell]
getCells (Row cells) = cells

data HeaderP = HasHeader | NoHeader | AutoHeader

csv HasHeader = do
  header <- row
  _ <- eol
  contents <- endBy row eol
  return (Doc (Just header) contents)
csv NoHeader = do
  contents <- endBy row eol
  return (Doc Nothing contents)
csv AutoHeader = undefined

row = fmap Row line
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell =
  do _ <- char '"'
     content <- many quotedChar
     _ <- char '"' <?> "quote at end of cell"
     return content

quotedChar =
  noneOf "\"" <|>
  try (string "\"\"" >> return '"')

eol = try (string "\n\r") <|>
      try (string "\r\n") <|>
      string "\n" <|>
      string "\r" <?>
      "end of line"

parseCSV :: HeaderP -> String -> Either ParseError Doc
parseCSV headerP input = parse (csv headerP) "(unknown)" input

(!) :: Row -> Int -> Cell
(Row cells) ! i = cells !! i
