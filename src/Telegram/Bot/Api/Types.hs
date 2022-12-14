{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Bot.Api.Types where

import Data.Aeson
  ( FromJSON (parseJSON)
  , KeyValue ((.=))
  , ToJSON (toJSON)
  , Value (Null)
  , object
  , withObject
  , (.:)
  , (.:?)
  )
import qualified Data.ByteString as B
import Data.List (find)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

newtype Token = Token Text deriving (Show)

instance IsString Token where
  fromString = Token . T.pack

data TelegramResponse a = TelegramResponse
  { ok :: Bool
  , result :: a
  }
  deriving (Eq, Show, Generic)

instance (FromJSON a) => FromJSON (TelegramResponse a) where
  parseJSON = withObject "TgResponse" $ \v ->
    TelegramResponse
      <$> v .: "ok"
      <*> v .: "result"

instance (ToJSON a) => ToJSON (TelegramResponse a) where
  toJSON (TelegramResponse ok res) =
    object ["ok" .= ok, "result" .= res]

data Update = Update
  { updateId :: Integer
  , message :: Maybe Message
  , inlineQuery :: Maybe InlineQuery
  , callbackQuery :: Maybe CallbackQuery
  }
  deriving (Eq, Show, Generic)

instance FromJSON Update where
  parseJSON = withObject "Update" $ \v ->
    Update
      <$> v .: "update_id"
      <*> v .:? "message"
      <*> v .:? "inline_query"
      <*> v .:? "callback_query"

instance ToJSON Update where
  toJSON (Update uid msg iq cbk) =
    object $ Prelude.filter ((/= Null) . snd) ["update_id" .= uid, "message" .= msg, "inline_query" .= iq, "callback_query" .= cbk]

data InlineQuery = InlineQuery
  { iqId :: Text
  , iqFrom :: User
  , iqQuery :: Text
  , iqOffset :: Text
  , iqChatType :: Maybe Text
  --, iqLocation :: ??? 
  }
  deriving (Eq, Show, Generic)

instance FromJSON InlineQuery where
  parseJSON = withObject "InlineQuery" $ \v ->
    InlineQuery
      <$> v .: "id"
      <*> v .: "from"
      <*> v .: "query"
      <*> v .: "offset"
      <*> v .:? "chat_type"

instance ToJSON InlineQuery where
  toJSON InlineQuery {..} =
    object $ Prelude.filter ((/= Null) . snd) ["id" .= iqId, "from" .= iqFrom, "query" .= iqQuery, "offset" .= iqOffset, "chat_type" .= iqChatType]

data AnswerInlineQueryRequest = AnswerInlineQueryRequest
  { aiqInlineQueryId :: Text
  , aiqResults :: [InlineQueryResult]
  }
  deriving (Eq,Show,Generic)

data InlineQueryResult =
    CachedMpeg4Gif Text Text
  | CachedGif Text Text
  deriving (Eq, Show, Generic)

instance ToJSON InlineQueryResult where
  toJSON (CachedGif iqId fileId) = object $ Prelude.filter ((/= Null) . snd) ["type" .= ("mpeg4_gif" :: Text), "id" .= iqId, "mpeg4_file_id" .= fileId]
  toJSON (CachedMpeg4Gif iqId fileId)  = object $ Prelude.filter ((/= Null) . snd) ["type" .= ("gif" :: Text), "id" .= iqId, "gif_file_id" .= fileId]

data CallbackQuery = CallbackQuery
  { cbkId :: Text
  , cbkFrom :: User
  , cbkMessage :: Maybe Message
  , cbkData :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON CallbackQuery where
  parseJSON = withObject "CallbackQuery" $ \v ->
    CallbackQuery
      <$> v .: "id"
      <*> v .: "from"
      <*> v .:? "message"
      <*> v .:? "data"

instance ToJSON CallbackQuery where
  toJSON CallbackQuery {..} =
    object $ Prelude.filter ((/= Null) . snd) ["id" .= cbkId, "from" .= cbkFrom, "message" .= cbkMessage, "data" .= cbkData]

data Message = Message
  { messageId :: Integer
  , from :: Maybe User
  , date :: Integer
  , chat :: Chat
  , replyToMessage :: Maybe Message
  , text :: Maybe Text
  , animation :: Maybe Animation
  , entities :: Maybe [MessageEntity]
  , replyMarkup :: Maybe InlineKeyboardMarkup
  }
  deriving (Eq, Show, Generic)

instance FromJSON Message where
  parseJSON = withObject "Message" $ \v ->
    Message
      <$> v .: "message_id"
      <*> v .:? "from"
      <*> v .: "date"
      <*> v .: "chat"
      <*> v .:? "reply_to_message"
      <*> v .:? "text"
      <*> v .:? "animation"
      <*> v .:? "entities"
      <*> v .:? "reply_markup"

instance ToJSON Message where
  toJSON Message {..} =
    object $ Prelude.filter ((/= Null) . snd) ["message_id" .= messageId, "from" .= from, "date" .= date, "chat" .= chat, "reply_to_message" .= replyToMessage, "text" .= text, "animation" .= animation, "entities" .= entities, "reply_markup" .= replyMarkup]

data Animation = Animation
  { animationId :: Text
  , animationUniqueId :: Text
  , animationWidth :: Integer
  , animationHeight :: Integer
  , animationDuration :: Integer
  } 
  deriving (Eq, Show, Generic)

instance FromJSON Animation where
  parseJSON = withObject "Animation" $ \v ->
    Animation
      <$> v .: "file_id"
      <*> v .: "file_unique_id"
      <*> v .: "width"
      <*> v .: "height"
      <*> v .: "duration"

instance ToJSON Animation where
  toJSON Animation {..} =
    object $ Prelude.filter ((/= Null) . snd) ["file_id" .= animationId, "file_unique_id" .= animationUniqueId, "width" .= animationWidth, "height" .= animationHeight, "duration" .= animationDuration]

data MessageEntity = MessageEntity
  { eType :: Text
  , eOffset :: Integer
  , eLength :: Integer
  , eUrl :: Maybe Text
  , eUser :: Maybe User
  , eLanguage :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON MessageEntity where
  parseJSON = withObject "MessageEntity" $ \v ->
    MessageEntity
      <$> v .: "type"
      <*> v .: "offset"
      <*> v .: "length"
      <*> v .:? "url"
      <*> v .:? "user"
      <*> v .:? "language"

instance ToJSON MessageEntity where
  toJSON MessageEntity {..} =
    object $ Prelude.filter ((/= Null) . snd) ["type" .= eType, "offset" .= eOffset, "length" .= eLength, "url" .= eUrl, "user" .= eUser, "language" .= eLanguage]

data User = User
  { userId :: Integer
  , isBot :: Bool
  , firstName :: Text
  , lastName :: Maybe Text
  , username :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON User where
  parseJSON = withObject "User" $ \v ->
    User
      <$> v .: "id"
      <*> v .: "is_bot"
      <*> v .: "first_name"
      <*> v .:? "last_name"
      <*> v .:? "username"

instance ToJSON User where
  toJSON User {..} =
    object $ Prelude.filter ((/= Null) . snd) ["id" .= userId, "is_bot" .= isBot, "first_name" .= firstName, "last_name" .= lastName, "username" .= username]

data Chat = Chat
  { chatId :: Integer
  , chatType :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Chat where
  parseJSON = withObject "Chat" $ \v ->
    Chat
      <$> v .: "id"
      <*> v .: "type"

instance ToJSON Chat where
  toJSON Chat {..} =
    object $ Prelude.filter ((/= Null) . snd) ["id" .= chatId, "type" .= chatType]

data SendPhotoRequest = SendPhotoRequest
  { pChatId :: Integer
  , content :: B.ByteString
  , pInlineKeyboard :: Maybe InlineKeyboardMarkup
  }
  deriving (Eq, Show)

data SendMessageRequest = SendMessageRequest
  { mChatId :: Integer
  , mText :: Text
  , mDisableNotification :: Bool
  , mReplyToMsgId :: Maybe Integer
  , mInlineKeyboard :: Maybe InlineKeyboardMarkup
  }
  deriving (Eq, Show)

data EditMessageRequest = EditMessageRequest
  { eChatId :: Integer
  , eMessageId :: Integer
  , eText :: Text
  , eInlineKeyboard :: Maybe InlineKeyboardMarkup
  }
  deriving (Eq, Show)

data EditMessageReplyMarkupRequest = EditMessageReplyMarkupRequest
  { rChatId :: Integer
  , rMessageId :: Integer
  , rInlineKeyboard :: Maybe InlineKeyboardMarkup
  }
  deriving (Eq, Show)

newtype InlineKeyboardMarkup = InlineKeyboardMarkup
  { inlineKeyboard :: [[InlineKeyboardButton]]
  }
  deriving (Eq, Show, Generic)

instance FromJSON InlineKeyboardMarkup where
  parseJSON = withObject "InlineKeyboardMarkup" $ \v ->
    InlineKeyboardMarkup
      <$> v .: "inline_keyboard"

instance ToJSON InlineKeyboardMarkup where
  toJSON (InlineKeyboardMarkup ik) =
    object ["inline_keyboard" .= ik]

data InlineKeyboardButton = InlineKeyboardButton
  { btnText :: Text
  , btnUrl :: Maybe Text
  , btnCbkData :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON InlineKeyboardButton where
  parseJSON = withObject "InlineKeyboardButton" $ \v ->
    InlineKeyboardButton
      <$> v .: "text"
      <*> v .:? "url"
      <*> v .:? "callback_data"

instance ToJSON InlineKeyboardButton where
  toJSON (InlineKeyboardButton txt url dat) =
    object $ Prelude.filter ((/= Null) . snd) ["text" .= txt, "url" .= url, "callback_data" .= dat]

getEntity :: Text -> Message -> Maybe Text
getEntity entityType (Message _ _ _ _ _ (Just txt) _ (Just entities) _) = do
  MessageEntity {..} <- find (\MessageEntity {..} -> eType == entityType) entities
  let substr o l t = T.take l $ T.drop o t
      entityTxt = substr (fromInteger eOffset) (fromInteger eLength) txt
  return entityTxt
getEntity _ _ = Nothing

getEntities :: Text -> Message -> [Text]
getEntities entityType (Message _ _ _ _ _ (Just txt) _ (Just entities) _) = do
  let substr o l t = T.take l $ T.drop o t
      entityTxt MessageEntity {..} = substr (fromInteger eOffset) (fromInteger eLength) txt
  entityTxt <$> filter (\MessageEntity {..} -> eType == entityType) entities
getEntities _ _ = []
