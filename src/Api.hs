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
import           Data.List                          (find)
import           Data.Maybe                         (listToMaybe)
import           Data.Text.Lazy                     (Text)
import           Database.PostgreSQL.Simple         (ConnectInfo (..),
                                                     Connection, Only (..),
                                                     Query, ToRow, connect,
                                                     defaultConnectInfo, query)
import           Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import           Database.PostgreSQL.Simple.ToField (ToField (..))
import           Database.PostgreSQL.Simple.ToRow   (ToRow (..))
import           Network.HTTP.Types.Status          (created201,
                                                     internalServerError500,
                                                     notAcceptable406,
                                                     notFound404)
import           Web.Scotty.Trans                   (ActionT, get, json, param,
                                                     params, post, scottyT,
                                                     status)

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

instance ToRow Post where
    toRow (Post _ pTitle pBody) = [toField pTitle, toField pBody]

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
        post "/" createAction
        get "/:id" showAction

indexAction :: Action
indexAction = do
    ps <- allPosts
    json $ object [ "posts" .= ps,
                    "meta" .= meta ps
                  ]
    where meta ps = object [ "count" .= length ps
                           ]

createAction :: Action
createAction = do
    prms <- params
    case (findVal "title" prms, findVal "body" prms) of
        (Just pTitle, Just pBody) -> do
            mp <- insertPost (Post Nothing pTitle pBody)
            case mp of
                Just p -> do
                    status created201
                    json $ object [ "post" .= p
                                  ]
                _ -> do
                    status internalServerError500
                    json ()
        _ -> do
            status notAcceptable406
            json ()
    where findVal key prms = fmap snd $ find (\(k, v) -> k == key) prms

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

insertPost :: Post -> ActionM (Maybe Post)
insertPost = fmap listToMaybe . runDB "insert into posts (title, body) values (?, ?) returning id, title, body"

findPost :: Integer -> ActionM (Maybe Post)
findPost id = fmap listToMaybe $ runDB "select id, title, body from posts where id = ?" (Only id)
