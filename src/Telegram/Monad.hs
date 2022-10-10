{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Telegram.Monad where

import App.ReplyM ( ReplyM, runReplyM )
import qualified Data.ByteString as B
import Data.Text (Text)
import Telegram.Bot.Api.Types ( Message (..), InlineKeyboardMarkup(..), InlineQueryResult )

data TelegramActionSimple = 
    SendMessage Integer Text
  | ReplyMessage Message Text
  | EditMessageText Message Text
  | EditMessageReplyMarkup Message
  | SendPhoto Integer B.ByteString
  | ReplyPhoto Message B.ByteString
  | AnswerInlineQuery Text [InlineQueryResult]
  deriving (Show)

data TelegramAction = WithKeyboard TelegramActionSimple InlineKeyboardMarkup | NoKeyboard TelegramActionSimple | NoAction

type TelegramM m a = ReplyM TelegramAction TelegramAction m a

runTelegramM :: Monad m => (TelegramAction -> m ()) -> TelegramM m TelegramAction -> m ()
runTelegramM f = runReplyM f f f
