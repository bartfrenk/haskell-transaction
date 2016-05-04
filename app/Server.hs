{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import Servant
import Transaction
import Network.Wai.Handler.Warp
import Network.Wai
import Control.Monad.Trans (liftIO)

type API = "account" :> Get '[JSON] [Transaction]

databasePath :: FilePath
databasePath = "test.db"

main :: IO ()
main = run 8081 app

getTransactions' :: IO [Transaction]
getTransactions' = do
  ts <- getTransactions databasePath
  -- putStrLn $ show (length ts)
  return ts

server :: Server API
server = liftIO $ getTransactions databasePath

api :: Proxy API
api = Proxy

app :: Application
app = serve api server
