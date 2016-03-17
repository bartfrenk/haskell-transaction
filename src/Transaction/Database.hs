{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-orphans #-}
module Transaction.Database where

import Control.Monad (when, forM_)

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Convertible

import Transaction.Types

-- | Orphan instance for Currency
instance Convertible Currency SqlValue where
  safeConvert (Currency decimal) = safeConvert (toRational decimal)
instance Convertible SqlValue Currency where
  safeConvert sqlValue = do
    x <- safeConvert sqlValue
    return (Currency (fromRational x))

instance Convertible Date SqlValue where
  safeConvert (Date day) = safeConvert day
instance Convertible SqlValue Date where
  safeConvert sqlValue = do
    x <- safeConvert sqlValue
    return (Date x)

connectDB :: FilePath -> IO Connection
connectDB path = do
  handle <- connectSqlite3 path
  prepareDB handle
  return handle

createTransactionTable :: IConnection c => c -> IO ()
createTransactionTable h = do
  do run h "create table transactions (\
           \trid integer not null primary key autoincrement,\
           \dest char not null,\
           \amount decimal not null,\
           \date data not null,\
           \category char,\
           \hash char blob not null)" []
  return ()

prepareDB :: IConnection c => c -> IO ()
prepareDB h = do
  tables <- getTables h
  when (not ("transactions" `elem` tables)) $ createTransactionTable h
  commit h


insertTransactions :: FilePath -> [Transaction] -> IO ()
insertTransactions path ts = do
  h <- connectDB path
  forM_ ts (insertTransaction h)
  commit h

getTransactions :: FilePath -> IO [Transaction]
getTransactions path = do
  h <- connectDB path
  r <- quickQuery' h "select * from transactions" []
  return (toTransaction <$> r)
  where
    toTransaction [_, sqlDest, sqlAmount, sqlDate, sqlCat, sqlHash] =
      Transaction (fromSql sqlDest) (fromSql sqlAmount) (fromSql sqlDate)
                  (fromSql sqlCat) (fromSql sqlHash)
    toTransaction _ = error "Could not convert to Transaction"


-- For now, there is no indication whether the transaction was
-- already stored. In the future a mechanism for getting that
-- information from IO might be useful.
insertTransaction :: IConnection c => c -> Transaction -> IO ()
insertTransaction h tr =
  run h "insert or ignore into transactions\
        \(dest, amount, date, category, hash)\
        \VALUES (?, ?, ?, ?, ?)" [toSql (trDest tr), toSql (trAmount tr),
                                  toSql (trDate tr), toSql (trCat tr),
                                  toSql (trHash tr)] >>
  return ()
