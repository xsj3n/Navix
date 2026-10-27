module Main where
import Navix.Log
import Data.List (isInfixOf)
import System.Exit (exitSuccess, exitFailure)

main :: IO ()
main = do
  logDebug "debug-marker"
  logInfo  "info-marker"
  logWarn  "warn-marker"
  logError "error-marker"
  logFatal "fatal-marker"

  contents <- readFile logFilePath
  let checks =
        [ ("Debug", "debug-marker")
        , ("Info",  "info-marker")
        , ("Warn",  "warn-marker")
        , ("Error", "error-marker")
        , ("Fatal", "fatal-marker")
        ]
      failures =
        [ (lvl, msg)
        | (lvl, msg) <- checks
        , not (("[" ++ lvl ++ "]") `isInfixOf` contents && msg `isInfixOf` contents)
        ]

  if null failures
    then putStrLn "[*] All log levels written and found in log file" >> exitSuccess
    else do
      putStrLn $ "[!] Missing or malformed log entries: " ++ show failures
      exitFailure
