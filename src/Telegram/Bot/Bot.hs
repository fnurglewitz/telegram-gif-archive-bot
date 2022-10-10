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
import App.ReplyM (dieIf, dieOnLeft, dieOnNothing, replyLog)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, handle)
import Control.Monad.Except
  ( MonadError (throwError)
  )
import Control.Monad.Reader
  ( MonadReader
  , ReaderT (runReaderT)
  , forever
  )
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection)
import qualified Database.Types as DB
import Logging.Logger (logGeneric)
import Logging.Types (HasLogger (logError, logInfo))
import Streaming
  ( Alternative ((<|>))
  , MonadIO (..)
  , MonadTrans (lift)
  , void
  )
import Telegram.Bot.Api.Client
  ( TelegramClient (editMessageText, editMessageReplyMarkup, sendMessage, answerInlineQuery, getUpdate)
  )
import Telegram.Bot.Api.Types
  ( CallbackQuery (..)
  , Chat (Chat, chatId, chatType)
  , EditMessageReplyMarkupRequest (EditMessageReplyMarkupRequest)
  , EditMessageRequest (EditMessageRequest)
  , InlineKeyboardButton (..)
  , InlineKeyboardMarkup (InlineKeyboardMarkup, inlineKeyboard)
  , Message (..)
  , Animation (..)
  , SendMessageRequest (SendMessageRequest)
  , AnswerInlineQueryRequest (..)
  , Update (Update, message)
  , User (userId)
  , getEntity, getEntities, InlineQuery (..), InlineQueryResult (CachedMpeg4Gif)
  )
import Telegram.Bot.Auth (checkAuth)
import Telegram.Monad
  ( TelegramAction (..)
  , TelegramActionSimple
      ( EditMessageText
      , EditMessageReplyMarkup
      , ReplyMessage
      , AnswerInlineQuery
      )
  , runTelegramM
  )
import Text.InterpolatedString.QM (qms)

type Effects m =
  ( MonadReader (AppCtx Connection) m
  , MonadIO m
  , MonadError Text m
  , TelegramClient m
  , DB.DB m
  , HasLogger m
  )

type Authorized m = ReaderT DB.Auth m

tshow :: Show a => a -> Text
tshow = T.pack . show

-- already in transformers 0.6.0.0 , not here
hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure

executeBotAction :: Effects m => TelegramAction -> m ()
executeBotAction (NoKeyboard (ReplyMessage Message {..} txt)) = sendMessage $ SendMessageRequest (chatId chat) txt True (Just messageId) Nothing
executeBotAction (WithKeyboard (ReplyMessage Message {..} txt) kb) = sendMessage $ SendMessageRequest (chatId chat) txt True (Just messageId) (Just kb)
executeBotAction (NoKeyboard (EditMessageText Message {..} txt)) = editMessageText $ EditMessageRequest (chatId chat) messageId txt Nothing
executeBotAction (WithKeyboard (EditMessageText Message {..} txt) kb) = editMessageText $ EditMessageRequest (chatId chat) messageId txt (Just kb)
executeBotAction (NoKeyboard (EditMessageReplyMarkup Message {..})) = editMessageReplyMarkup $ EditMessageReplyMarkupRequest (chatId chat) messageId Nothing
executeBotAction (WithKeyboard (EditMessageReplyMarkup Message {..}) kb) = editMessageReplyMarkup $ EditMessageReplyMarkupRequest (chatId chat) messageId (Just kb)
executeBotAction (NoKeyboard (AnswerInlineQuery iqId rs)) = answerInlineQuery $ AnswerInlineQueryRequest iqId rs
executeBotAction NoAction = pure ()
executeBotAction _ = error "Unimplemented action"

runBot :: Effects m => m ()
runBot = do
  mbUpdate <- getUpdate
  case selectAction =<< mbUpdate of
    Nothing -> throwError "No update found, fuck you"
    Just x -> x
  where
    selectAction (Update _ msg iq _) = handleMessage <$> msg <|> handleInlineQuery <$> iq

startBot :: AppCtx Connection -> IO ()
startBot ctx@AppCtx {..} = do
  logGeneric logger "INFO" config "Starting telegram bot"
  forever $ handle
    do (\e -> logGeneric logger "ERROR" config $ tshow (e :: SomeException))
    do
      r <- runExceptT $ runReaderT (runAppM runBot) ctx
      case r of
        Left err -> logGeneric logger "ERROR" config err
        Right x -> pure x
      threadDelay 10000

handleMessage :: Effects m => Message -> m ()
handleMessage m@Message {..} = void $ runMaybeT do
  user <- hoistMaybe from
  auth <- hoistMaybe =<< lift (checkAuth user)
  cmd <- hoistMaybe $ getEntity "bot_command" m
  lift $ flip runReaderT auth $ handleCommand cmd m

handleCommand :: Effects m => Text -> Message -> Authorized m ()
handleCommand "/save" = saveCommand
handleCommand cmd = \m -> lift $
  runTelegramM executeBotAction $
    do pure $ NoKeyboard $ ReplyMessage m [qms|Unknown command: {cmd}|]

saveCommand :: Effects m => Message -> Authorized m ()
saveCommand m = do
  lift $
    runTelegramM executeBotAction $ do
      liftIO $ print m
      rtm@Message {..} <- dieOnNothing (replyToMessage m) $ NoKeyboard $ ReplyMessage m "reply to a message, moron"
      ani@Animation{..} <- dieOnNothing animation $ NoKeyboard $ ReplyMessage m "reply to a message with an animation, moron"
      liftIO $ print ani
      let hashtags = getEntities "hashtag" m
      dieIf
        do null hashtags
        do NoKeyboard $ ReplyMessage m "reply to a message with an animation and add some hashtags, moron"
      lift $ DB.saveGif animationId hashtags
      lift $ logInfo [qms|Bullshit saved: {animationId} - {hashtags}|]
      pure NoAction

handleInlineQuery :: Effects m => InlineQuery -> m ()
handleInlineQuery iq@InlineQuery{..} = runTelegramM executeBotAction $ do  
  let hashtags = filter check $ T.splitOn " " iqQuery
  gifs <- lift $ DB.findGifsByTags hashtags
  dieIf
    do null hashtags
    do NoAction
  pure $ NoKeyboard $ AnswerInlineQuery iqId $ convert <$> gifs 
  where
    check txt = T.take 1 txt == "#"
    convert DB.SavedGif{..} = CachedMpeg4Gif (T.take 64 gifId) gifId

