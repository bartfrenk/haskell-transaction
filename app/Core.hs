{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Core where

import Utils
import Control.Concurrent.STM
import Transaction
import Control.Monad.Reader

data AppData = AppData {
  transactionsT :: TVar [Transaction]
  }

newtype AppM a = AppM (ReaderT AppData IO a)
               deriving (Functor, Applicative, Monad, MonadIO,
                         MonadReader AppData)

runAppM :: AppM a -> AppData -> IO a
runAppM (AppM app) appData = (runReaderT app appData)

initAppData :: IO AppData
initAppData = AppData <$> newTVarIO []

appendTransactions :: [FilePath] -> AppM ()
appendTransactions paths = do
  var <- transactionsT <$> ask
  ts <- liftIO $ loadTransactionFiles paths
  liftIO $ atomically  $ modifyTVar var (++ts)
  return ()
