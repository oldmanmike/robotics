module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Robotics.NXT
import Bot.Logger
import Bot.Logic


runBot :: Logger -> IO ()
runBot logger = do
    --writeTo logger Info $ "Start Value Calibrated to: " ++ show startValue
    -- threadDelay 1000000
    lineValue <- withNXT defaultDevice $ calibrateAVG Three 100
    writeTo logger Info $ "Line Value Calibrated to: " ++ show lineValue
    threadDelay 1000000
    withNXT defaultDevice $ forever $ do
      followLineWith logger lineValue


main :: IO ()
main = do
  logger <- newLogger defaultBufSize "line-follower.log" Debug 
  writeTo logger Info "Welcome to the Line Follower!"
  runBot logger
