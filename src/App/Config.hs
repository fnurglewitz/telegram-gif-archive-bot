{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Config (Config (..), TelegramConfig (..), DatabaseConfig (..), AppCtx (..), cfgParserInfo) where

import Control.Concurrent.MVar (MVar)
import Data.Text (Text)
import Options.Applicative
  ( Parser,
    ParserInfo,
    auto,
    help,
    helper,
    idm,
    info,
    long,
    metavar,
    option,
    showDefault,
    strOption,
    value,
    (<**>),
  )
import System.Log.FastLogger (LoggerSet)
import Telegram.Bot.Api.Types (Token)

data Config = Config
  { env :: Text,
    appName :: Text,
    appVersion :: Text,
    logLevel :: Text,
    telegramCfg :: TelegramConfig,
    databaseCfg :: DatabaseConfig,
    webhookCfg :: WebhookConfig
  }
  deriving (Show)

data TelegramConfig = TelegramConfig
  { tgBaseUrl :: Text,
    tgToken :: Token,
    tgAdmin :: Integer
  }
  deriving (Show)

data DatabaseConfig = DatabaseConfig
  { pgHost :: Text,
    pgPort :: Integer,
    pgUser :: Text,
    pgPass :: Text,
    pgDb :: Text
  }
  deriving (Show)

data WebhookConfig = WebhookConfig
  { whUrl :: Text,
    whCert :: Text,
    whSecret :: Text
  }
  deriving (Show)

data AppCtx a = AppCtx
  { config :: Config,
    logger :: LoggerSet,
    db :: a,
    lastTelegramUpdateId :: MVar Integer
  }

instance Show (AppCtx a) where
  show AppCtx {..} = show config -- TODO: ?

tgParser :: Parser TelegramConfig
tgParser =
  TelegramConfig
    <$> strOption
      ( long "tg-base-url"
          <> metavar "TGBASEURL"
          <> showDefault
          <> value "https://api.telegram.org/bot"
          <> help "Telegram bot api base url"
      )
    <*> strOption
      ( long "tg-token"
          <> metavar "TGTOKEN"
          <> help "Telegram bot api token"
      )
    <*> option
      auto
      ( long "tg-admin"
          <> help "Telegram bot admin user"
          <> showDefault
          <> metavar "TGADMIN"
      )

dbParser :: Parser DatabaseConfig
dbParser =
  DatabaseConfig
    <$> strOption
      ( long "pg-host"
          <> metavar "PGHOST"
          <> showDefault
          <> value "localhost"
          <> help "Postgres host"
      )
    <*> option
      auto
      ( long "pg-port"
          <> help "Postgres port"
          <> showDefault
          <> value 5432
          <> metavar "PGPORT"
      )
    <*> strOption
      ( long "pg-user"
          <> metavar "PGUSER"
          <> showDefault
          <> value "postgres"
          <> help "Postgres user"
      )
    <*> strOption
      ( long "pg-pass"
          <> metavar "PGPASS"
          <> showDefault
          <> value "password"
          <> help "Postgres password"
      )
    <*> strOption
      ( long "pg-db"
          <> metavar "PGDB"
          <> showDefault
          <> value "pod"
          <> help "Postgres database name"
      )

whParser :: Parser WebhookConfig
whParser =
  WebhookConfig
    <$> strOption
      ( long "wh-url"
          <> metavar "WHURL"
          <> showDefault
          <> value "http://bot.pwned.it"
          <> help "Webhook url"
      )
    <*> strOption
      ( long "wh-cert"
          <> metavar "WHCERT"
          <> showDefault
          <> value "/etc/letsencrypt/archive/pwned.it/cert1.pem"
          <> help "Webhook certificate path"
      )
    <*> strOption
      ( long "pg-user"
          <> metavar "PGUSER"
          <> showDefault
          <> value "postgres"
          <> help "Postgres user"
      )

cfgParser :: Parser Config
cfgParser =
  Config
    <$> strOption
      ( long "env"
          <> metavar "ENV"
          <> showDefault
          <> value "dev"
          <> help "App environment"
      )
    <*> strOption
      ( long "appname"
          <> metavar "APPNAME"
          <> showDefault
          <> value "pmt"
          <> help "App name"
      )
    <*> strOption
      ( long "appversion"
          <> metavar "APPVERSION"
          <> showDefault
          <> value "0.0.1" -- TODO: you know
          <> help "App version"
      )
    <*> strOption
      ( long "loglevel"
          <> metavar "LOGLEVEL"
          <> showDefault
          <> value "INFO"
          <> help "Log level (ERROR | INFO | DEBUG)"
      )
    <*> tgParser
    <*> dbParser
    <*> whParser

cfgParserInfo :: ParserInfo Config
cfgParserInfo = info (cfgParser <**> helper) idm
