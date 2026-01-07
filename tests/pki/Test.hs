module Main where
import Navix.PKI
import Data.X509.Validation
import Control.Monad (when)
import System.Exit (exitSuccess, exitFailure)
import Data.Foldable (for_)

main :: IO ()
main = do
  initPKI
  reasons <- verifyPKI "Navix.local"
  when (null reasons) (putStrLn "[*] Validation completed, all checks passed" >> exitSuccess)
  printAllReasons reasons
  exitFailure
  
