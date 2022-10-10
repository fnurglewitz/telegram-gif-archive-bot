{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Bot.Auth (checkAuth) where

import App.Config
  ( AppCtx (..)
  , Config (telegramCfg)
  , TelegramConfig (tgAdmin)
  )
import Control.Monad.Reader (MonadIO (..), MonadReader (ask))
import Data.Maybe (fromMaybe)
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.Types as DB
import Logging.Types (HasLogger (logInfo))
import Telegram.Bot.Api.Client (TelegramClient (sendMessage))
import Telegram.Bot.Api.Types
  ( SendMessageRequest (SendMessageRequest)
  , User (..)
  )

checkAuth :: (MonadReader (AppCtx Connection) m, MonadIO m, TelegramClient m, DB.DB m, HasLogger m) => User -> m (Maybe DB.Auth)
checkAuth User {..} = do
  AppCtx {..} <- ask
  let adminId = tgAdmin . telegramCfg $ config
  if userId == adminId
    then do 
      logInfo "admin account detected"
      return . return $ DB.Auth adminId undefined True "admin" -- crap
    else do
      mbAuth <- DB.getAuth userId
      case mbAuth of
        Nothing -> do
          now <- liftIO getCurrentTime
          logInfo "miiiinghie"
          DB.saveAuth $ DB.Auth userId now False (fromMaybe "" username) -- TODO
          sendMessage $ SendMessageRequest userId "Unauthorized" True Nothing Nothing
          return Nothing
        Just auth@DB.Auth {..} -> do
          logInfo "swag"
          if aEnabled
            then return . return $ auth
            else do
              sendMessage $ SendMessageRequest userId "Unauthorized" True Nothing Nothing
              return Nothing
