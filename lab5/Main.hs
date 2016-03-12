module Main where

import Control.Monad
import Control.Monad.IO.Class
import Robotics.NXT

runLoop :: NXT ()
runLoop = do
    setOutputState A 100 [MotorOn,Brake] RegulationModeIdle 0 MotorRunStateRunning 0
    setOutputState B 100 [MotorOn,Brake] RegulationModeIdle 0 MotorRunStateRunning 0

main :: IO ()
main = do
    withNXT defaultDevice $ do
      liftIO $ print "Hello"
      forever runLoop
    return ()
