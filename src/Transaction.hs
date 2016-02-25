module Transaction
    ( Account, Transaction, ParseError, Scheme(..),
      parseTransactions, selectScheme,
      trDest, trAmount, trDate, trCat, trMonth, trYear )
    where

import Transaction.Scheme
import Transaction.Types
import Transaction.CSV (ParseError, csvContents, parseCSV)

parseTransactions :: Scheme -> String -> Either ParseError [Transaction]
parseTransactions scheme input = do
  doc <- parseCSV (schemeHeaderP scheme) input
  return $ (schemeMap scheme) `map` (csvContents doc)
