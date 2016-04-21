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
import Data.Time.Calendar (fromGregorian)
import Data.ByteString.Char8 (pack)
import Network.Wai.Handler.Warp
import Network.Wai

type API = "account" :> Get '[JSON] [Transaction]

main :: IO ()
main = run 8081 app

server :: Server API
server = return [Transaction "A" amount (Date day) Nothing hash]
  where day = fromGregorian 2016 2 3
        amount = Currency 10
        hash = pack "Hello"

api :: Proxy API
api = Proxy

app :: Application
app = serve api server
