{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Api
    ( api
    ) where

import           Control.Monad.IO.Class             (MonadIO, liftIO)
import           Control.Monad.Reader               (MonadReader, ReaderT, asks,
                                                     runReaderT)
import           Control.Monad.Trans.Class          (lift)
import           Data.Aeson                         (ToJSON (..), object, (.=))
import           Data.Maybe                         (listToMaybe)
import           Data.Text.Lazy                     (Text)
import           Database.PostgreSQL.Simple         (ConnectInfo (..),
                                                     Connection, Only (..),
                                                     Query, ToRow, connect,
                                                     defaultConnectInfo, query)
import           Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import           Network.HTTP.Types.Status          (notFound404)
import           Web.Scotty.Trans                   (ActionT, get, json, param,
                                                     scottyT, status)

data Config = Config
              { dbConn :: Connection
              }

newtype ConfigM a = ConfigM
                 { runConfigM :: ReaderT Config IO a
                 } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

type ActionM a = ActionT Text ConfigM a
type Action = ActionM ()

data Post = Post
            { postId    :: Maybe Integer,
              postTitle :: Text,
              postBody  :: Text
            }
            deriving (Show)

instance ToJSON Post where
    toJSON (Post mpId pTitle pBody) =
        object [ "id" .= mpId,
                 "title" .= pTitle,
                 "body" .= pBody
               ]

instance FromRow Post where
    fromRow = Post <$> field <*> field <*> field

getConfig :: IO Config
getConfig = do
    conn <- connect defaultConnectInfo { connectDatabase = "hs-api-scotty",
                                         connectUser = "mats"
                                       }
    return $ Config conn

api :: IO ()
api = do
    conf <- getConfig
    let r m = runReaderT (runConfigM m) conf
    scottyT 3000 r $ do
        get "/" indexAction
        get "/:id" showAction

indexAction :: Action
indexAction = do
    ps <- allPosts
    json $ object [ "posts" .= ps,
                    "meta" .= meta ps
                  ]
    where meta ps = object [ "count" .= length ps
                           ]

showAction :: Action
showAction = do
    id :: Integer <- param "id"
    mp <- findPost id
    case mp of
        Just p -> json $ object [ "post" .= p
                                ]
        Nothing -> do
            status notFound404
            json ()

runDB :: (ToRow p, FromRow r) => Query -> p -> ActionM [r]
runDB q p = do
    conn <- lift $ asks dbConn
    liftIO $ query conn q p

allPosts :: ActionM [Post]
allPosts = runDB "select id, title, body from posts" ()

findPost :: Integer -> ActionM (Maybe Post)
findPost id = fmap listToMaybe $ runDB "select id, title, body from posts where id = ?" (Only id)
