module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Robotics.NXT
import Bot.Logger
import Bot.Logic


runBot :: Logger -> IO ()
runBot logger = do
    withNXT defaultDevice $ 
      initialLeap logger >> (forever $ followLineWith logger)

initialLeap :: Logger -> NXT ()
initialLeap logger = do
    setOutputStateConfirm A 50 [MotorOn,Brake,Regulated] RegulationModeIdle 0 MotorRunStateRunning 0
    setOutputStateConfirm B 50 [MotorOn,Brake,Regulated] RegulationModeIdle 0 MotorRunStateRunning 0
    liftIO $ threadDelay 1000000
    liftIO $ writeTo logger Info "Beginning line follow..."

main :: IO ()
main = do
  logger <- newLogger defaultBufSize "line-follower.log" Debug 
  writeTo logger Info "Welcome to the Line Follower!"
  runBot logger
