
module Main where

-- import qualified HOASCBV
-- import qualified HOASCBN
import qualified Interp

main :: IO ()
main = do
  -- putStrLn "HOAS call-by-value"
  -- putStrLn "----------------------------------------"
  -- HOASCBV.bench

  -- putStrLn "HOAS call-by-need"
  -- putStrLn "----------------------------------------"
  -- HOASCBN.bench

  putStrLn "Interpreted call-by-value"
  putStrLn "----------------------------------------"
  Interp.bench
