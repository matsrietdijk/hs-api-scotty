{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api
    ( api
    ) where

import Web.Scotty (ActionM, scotty, get, json, param)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)

api :: IO ()
api = scotty 3000 $ do
    get "/" homeAction
    get "/:id" showAction

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
showAction :: ActionM ()
showAction = do
    _id :: Integer <- param "id"
    json $ object [ "post" .= single_post
                  ]
    where single_post = Post (Just 1) "title 1" "body 1"
