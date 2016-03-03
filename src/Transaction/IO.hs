module Transaction.IO where

import Transaction.Scheme (Scheme(..), selectScheme)
import Transaction.CSV (ParseError, parseCSV, csvContents)
import Transaction.Types (Transaction)
import Utils (loadFile, injectNothingAs, injectErrorAs)
import Control.Monad (join)

data LoadError = FileNotFound | InvalidCSV ParseError | UnknownScheme
               deriving (Show, Eq)

-- |Concatenates transactions succesfully loaded from specified paths.
loadTransactionFiles :: [FilePath] -> IO [Transaction]
loadTransactionFiles paths = do
  tss <- mapM loadTransactionFile paths
  return $ concat (map removeFailed tss)
  where removeFailed (Right ts) = ts
        removeFailed (Left _) = []

-- |Loads transactions from the CSV file at the specified path.
loadTransactionFile :: FilePath -> IO (Either LoadError [Transaction])
loadTransactionFile path = fmap (join . (parseTs <$> scheme <*>)) input
  where scheme = injectNothingAs UnknownScheme $ selectScheme path
        input = injectNothingAs FileNotFound <$> loadFile path
        parseTs sch inp = injectErrorAs InvalidCSV (parseTransactions sch inp)

-- |Deserializes a string of transaction according to specified scheme.
parseTransactions :: Scheme -> String -> Either ParseError [Transaction]
parseTransactions scheme input = do
  doc <- parseCSV (schemeHeaderP scheme) input
  return $ (schemeMap scheme) `map` (csvContents doc)

