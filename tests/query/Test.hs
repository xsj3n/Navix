module Main where
import Navix.DatabaseInterface
import Data.Foldable (for_)
import Control.Monad (forM_)


main :: IO ()
main = do
  print "[*] Starting query file reading test..."
  readQueryFiles "./queries/init/" >>= print
  readQueryFile "./queries/register.sql" >>= print 
  pure ()
