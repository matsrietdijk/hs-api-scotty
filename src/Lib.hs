{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( api
    ) where

import Web.Scotty
import Data.Aeson hiding (json)
import Data.Text hiding (length)

api :: IO ()
api = scotty 3000 $ do
    get "/" homeAction

data Post = Post
            { postId :: Maybe Integer,
              postTitle :: Text,
              postBody :: Text
            }
            deriving (Show)

instance ToJSON Post where
    toJSON (Post mpId pTitle pBody) =
        object [ "id" .= mpId,
                 "title" .= pTitle,
                 "body" .= pBody
               ]

homeAction :: ActionM ()
homeAction = json $ object [ "posts" .= posts,
                             "meta" .= meta
                           ]
    where posts = [Post (Just 1) "title 1" "body 1"]
          meta = object [ "count" .= length posts
                        ]
