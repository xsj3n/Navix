module Navix.Log where

import System.IO (IOMode (AppendMode), hClose, hPutStrLn, openFile)
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)

data LogLevel = Debug | Info | Warn | Error | Fatal
  deriving (Eq, Ord, Show)

logFilePath :: FilePath
logFilePath = "./navix.log"

logMessage :: LogLevel -> String -> IO ()
logMessage level msg = do
  timestamp <- formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" <$> getZonedTime
  handle    <- openFile logFilePath AppendMode
  hPutStrLn handle $ "[" ++ timestamp ++ "] [" ++ show level ++ "] " ++ msg
  hClose handle

logDebug :: String -> IO ()
logDebug = logMessage Debug

logInfo :: String -> IO ()
logInfo = logMessage Info

logWarn :: String -> IO ()
logWarn = logMessage Warn

logError :: String -> IO ()
logError = logMessage Error

logFatal :: String -> IO ()
logFatal = logMessage Fatal
