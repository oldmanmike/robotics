module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Robotics.NXT
import Bot.Logger
import Bot.Logic


runBot :: Logger -> IO ()
runBot logger = do
    withNXT defaultDevice $ forever $ do
      -- threshold1 <- calibrateAVG Three 10
      -- threshold2 <- calibrateAVG Four 10
      --rotateMotor A 50
      --rotateMotor B 50
      --stopEverything
      followLineWith logger threshold1 threshold2


main :: IO ()
main = do
  logger <- newLogger defaultBufSize "line-follower.log" Debug 
  writeTo logger Info "Welcome to the Line Follower!"
  runBot logger
