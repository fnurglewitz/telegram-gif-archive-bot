{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module App.AppM.Database where

import App.Config (DatabaseConfig (..), db)
import App.AppM.Type (AppM)
import Control.Monad (void)
import Control.Monad.Reader (MonadIO, asks, liftIO)
import Data.Functor ((<&>))
import Data.Maybe (listToMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple
  ( ConnectInfo (..)
  , Connection
  , In (In)
  , Only (fromOnly)
  , connect
  , execute
  , executeMany
  , execute_
  , query
  , query_
  )
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.Types
  ( Auth
      ( Auth
      , aEnabled
      , aRequestTs
      , aUserId
      , aUsername
      )
  , DB (..)
  , SavedGif(..) 
  )
import Database.PostgreSQL.Simple.Types (PGArray(PGArray))

getConnection :: DatabaseConfig -> IO Connection
getConnection DatabaseConfig {..} = connect $ ConnectInfo (T.unpack pgHost) 5432 (T.unpack pgUser) (T.unpack pgPass) (T.unpack pgDb)

instance (MonadIO m) => DB (AppM Connection e m) where
  saveAuth Auth {..} = do
    conn <- asks db
    void $
      liftIO $
        execute
          conn
          [sql|INSERT INTO public.auth (user_id, request_ts, enabled, username) VALUES (?, ?, ?, ?)|]
          (aUserId, aRequestTs, aEnabled, aUsername)

  getAuth uid = do
    conn <- asks db
    r <- liftIO $ query conn "select user_id, request_ts, enabled, username from public.auth where user_id = ? limit 1" [uid]
    return $ listToMaybe r

  saveGif gid tags = do
    conn <- asks db
    uuid <- liftIO nextRandom
    void $
        liftIO $
          execute
            conn
            [sql|INSERT INTO public.gif (gif_id, tags, uuid) VALUES(?, ?, ?) on conflict (gif_id) do update set tags = ?|]
            (gid, PGArray tags, uuid, PGArray tags)

  findGifsByTags tags = do
    conn <- asks db
    liftIO $ query conn "select gif_id, tags, uuid from public.gif where tags && ?" [PGArray tags]
