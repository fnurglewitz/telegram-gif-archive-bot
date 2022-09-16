{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import App.Config
import App.AppM
import App.AppM.Database ( getConnection )
import App.AppM.Logging ( getLoggerSet )
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (throwIO)
import Control.Monad
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
import Telegram.Bot.Bot

main :: IO ()
main = do
  cfg@Config {..} <- customExecParser p cfgParserInfo
  conn <- getConnection databaseCfg
  loggerSet <- getLoggerSet
  luid <- newMVar (0 :: Integer)
  let ctx = AppCtx cfg loggerSet conn luid 
  print cfg
  startBot ctx
  where
    p = prefs $ showHelpOnEmpty <> showHelpOnError
