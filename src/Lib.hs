{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( api
    ) where

import Web.Scotty

api :: IO ()
api = scotty 3000 $ do
    get "/" homeAction

homeAction :: ActionM ()
homeAction = html "Hello World"
