
module Common where

import Data.Time.Clock
import Control.Monad
import System.IO

timed :: Show b => String -> Int -> (a -> IO b) -> a -> IO ()
timed msg times act a = do
  buffering <- hGetBuffering stdout
  hSetBuffering stdout NoBuffering
  putStrLn msg
  putStrLn $ "Iterations: " ++ show times
  t0 <- getCurrentTime
  replicateM_ times $ do
    b <- act a
    putStr $ show b ++ " "
  t1 <- getCurrentTime
  putStrLn $ "\nAverage time: " ++ show (diffUTCTime t1 t0 / fromIntegral times)
  hSetBuffering stdout buffering
{-# noinline timed #-}
