module Main where

import Control.Monad
import Control.Monad.IO.Class
import Robotics.NXT

runLoop :: Int -> Int -> NXT ()
runLoop threshold reading = do
    if reading >= threshold
      then do
        setOutputState A (-100) [MotorOn,Brake] RegulationModeMotorSpeed 0 MotorRunStateRunning 0
        setOutputState B 100 [MotorOn,Brake] RegulationModeMotorSpeed 0 MotorRunStateRunning 0
      else do
        setOutputState A 0 [MotorOn,Brake] RegulationModeMotorSpeed 0 MotorRunStateIdle 0
        setOutputState B 0 [MotorOn,Brake] RegulationModeMotorSpeed 0 MotorRunStateIdle 0



main :: IO ()
main = do
    withNXT defaultDevice $ do
      forever runLoop 0 1
    return ()
