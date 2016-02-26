module Main where

import Control.Monad
import Data.List
import Robotics.NXT

rotateMotor :: OutputPort -> OutputPower -> NXT ()
rotateMotor port power = setOutputStateConfirm port power [MotorOn, Brake,Regulated] RegulationModeIdle 0 MotorRunStateRunning 0

getScaledValue :: InputValue -> ScaledValue
getScaledValue (InputValue _ _ _ _ _ _ _ x _) = x

average :: (Real a, Integral b) => [a] -> b
average xs = round (realToFrac (sum xs) / genericLength xs)

sorter :: ScaledValue -> NXT ()
sorter ambientThreshold = do
    setInputModeConfirm Two Switch BooleanMode
    setInputModeConfirm Three LightActive PctFullScaleMode
    v <- getInputValues Three
    let colorValue = getScaledValue v
    if colorValue > ambientThreshold
        then if colorValue >= 50 -- 47
                then rotateMotor A 50 >> stopEverything
                else rotateMotor A (-50) >> stopEverything
        else stopEverything
    stopEverything

calibrate :: NXT ScaledValue
calibrate = do
    setInputModeConfirm Three LightActive PctFullScaleMode
    lst <- replicateM 100 (getInputValues Three)
    return (foldr1 max (map getScaledValue lst))
    
main :: IO ()
main = do
    withNXT defaultDevice $ do 
        threshold <- calibrate
        forever (sorter threshold)
    return ()
