{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Api where

import Data.Text (Text)
import Servant

type Api =
       AppApi
  :<|> Raw

type AppApi =
     "api" :> "data" :>
          (     Get '[PlainText] Text 
          :<|> ReqBody '[PlainText] Text :> Post '[PlainText] Text
          )

fullApi :: Proxy Api
fullApi = Proxy

appApi :: Proxy AppApi
appApi = Proxy