{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( api
    ) where

import Web.Scotty
import Data.Text

api :: IO ()
api = scotty 3000 $ do
    get "/" homeAction

homeAction :: ActionM ()
homeAction = json ("Hello World" :: Text)
