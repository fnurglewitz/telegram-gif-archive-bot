{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Types where

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.Types (PGArray(..))
import GHC.Generics (Generic)

data Auth = Auth
  { aUserId :: Integer
  , aRequestTs :: UTCTime
  , aEnabled :: Bool
  , aUsername :: Text
  }
  deriving (Generic, FromRow, ToRow, Show)

data SavedGif = SavedGif
  { gifId :: Integer
  , gifTags :: PGArray Text
  , gifCreatedAt :: UTCTime
  }
  deriving (Generic, FromRow, ToRow, Show)

class (Monad m) => DB m where
  saveAuth :: Auth -> m ()
  getAuth :: Integer -> m (Maybe Auth)

  saveGif :: Text -> [Text] -> m ()
  findGifsByTags :: [Text] -> m [SavedGif]
