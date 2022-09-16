{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Telegram.Bot.Bot where

import App.AppM (AppM (runAppM))
import App.Config (AppCtx (..))
import App.ReplyM (dieIf, dieOnLeft, dieOnNothing)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, handle)
import Control.Monad.Except
  ( MonadError (throwError)
  )
import Control.Monad.Reader
  ( MonadReader (ask)
  , ReaderT (runReaderT)
  , forever
  )
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Char (digitToInt)
import Data.List (find, nub)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.Types as DB
import Logging.Logger (logGeneric)
import Logging.Types (HasLogger (logError, logInfo))
import Network.Wai.Handler.Warp
import Servant
import Servant.Auth.Server
import Servant.Server
import Streaming
  ( Alternative ((<|>))
  , MonadIO (..)
  , MonadTrans (lift)
  , void
  )
import Telegram.Bot.Api.Client
  ( TelegramClient (editMessage, editMessageReplyMarkup, sendMessage)
  )
import Telegram.Bot.Api.Types
  ( CallbackQuery (..)
  , Chat (Chat, chatId, chatType)
  , EditMessageReplyMarkupRequest (EditMessageReplyMarkupRequest)
  , EditMessageRequest (EditMessageRequest)
  , InlineKeyboardButton (..)
  , InlineKeyboardMarkup (InlineKeyboardMarkup, inlineKeyboard)
  , Message (..)
  , SendMessageRequest (SendMessageRequest)
  , Update (Update)
  , User (userId)
  , getEntity
  )
-- import Telegram.Bot.Auth (checkAuth)
import Telegram.Monad
  ( TelegramAction (..)
  , TelegramActionSimple
      ( EditMessage
      , EditMessageReplyMarkup
      , ReplyMessage
      )
  , runTelegramM
  )
import Text.InterpolatedString.QM (qms)
import Servant (Context(EmptyContext))

import Control.Exception
import GHC.Base (undefined)
type Effects m =
  ( MonadReader (AppCtx Connection) m
  , MonadIO m
  , MonadError ServerError m
  , TelegramClient m
  , DB.DB m
  , HasLogger m
  )

type WebhookAPI = "webhook" :> ReqBody '[JSON] Update :> Post '[JSON] ()

webhookAPI :: Proxy WebhookAPI
webhookAPI = Proxy

server :: Effects m => ServerT WebhookAPI (AppM Connection ServerError m)
server = processUpdate

processUpdate :: Effects m => Update ->  m ()
processUpdate (Update uid msg iq cbk) = do
  -- ctx@AppCtx {..} <- ask
  liftIO $ logInfo "ciaoo"
  -- void $ throwError  err500

startBot :: AppCtx Connection -> IO ()
startBot ctx@AppCtx {..} = do
  logGeneric logger "INFO" config "Starting telegram bot"
  run 9090 $ serve webhookAPI $ hoistServer webhookAPI (r ctx) server
   where
     r c x = do
       e <- runExceptT $ runReaderT (runAppM x) c
       case e of
         Left _ -> throwError err500
         Right x -> pure x
