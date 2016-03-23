module Bot.Logic
  ( move
  , runBot
  ) where

import Control.Monad
import Robotics.NXT


move :: Int -> Int -> NXT ()
move threshold reading = do
    if reading >= threshold
      then do
        setOutputState A (-100) [MotorOn,Brake] RegulationModeMotorSpeed 0 MotorRunStateRunning 0
        setOutputState B 100 [MotorOn,Brake] RegulationModeMotorSpeed 0 MotorRunStateRunning 0
      else do
        setOutputState A 0 [MotorOn,Brake] RegulationModeMotorSpeed 0 MotorRunStateIdle 0
        setOutputState B 0 [MotorOn,Brake] RegulationModeMotorSpeed 0 MotorRunStateIdle 0


runBot :: IO ()
runBot = do
    _ <-  withNXT defaultDevice $ do
            forever $ move 0 1
    return ()

