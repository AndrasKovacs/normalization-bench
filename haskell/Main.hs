
module Main where

import qualified HOASCBV
import qualified HOASCBN

main :: IO ()
main = do
  putStrLn "Call-by-value"
  putStrLn "----------------------------------------"
  HOASCBV.bench
  putStrLn "Call-by-need"
  putStrLn "----------------------------------------"
  HOASCBN.bench
