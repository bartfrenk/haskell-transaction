{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Core where

import Utils (gather)
import Transaction
import Control.Concurrent.STM
import Data.Map.Lazy (toList)
import Control.Monad.Reader
import Data.Monoid (Sum(..))

type AccountStats = (Account, Sum Integer, Currency, Currency)

allAccountStats :: [Transaction] -> [AccountStats]
allAccountStats = flatList . (gather trDest accountStats)
  where accountStats tr = (1, trDebet tr, trCredit tr)
        flatList as = flatten <$> toList as
        flatten (k, (n, d, c)) = (k, n, d, c)

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
