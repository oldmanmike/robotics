module Bot.Logic
  ( calibrateMAX
  , calibrateAVG
  , maybeTurn
  , followLineWith
  , readSensor
  , rotateMotor 
  , getScaledValue 
  ) where


import Control.Monad
import Control.Monad.IO.Class
import Robotics.NXT
import Bot.Logger


-- | Takes n readings of the environment with the light sensor and returns 
-- the largest reading of them all. Used to set a good threshold for the 
-- sensor later on to determine whether there's a brick in front of it in 
-- the first place.
calibrateMAX :: InputPort -> Int -> NXT ScaledValue
calibrateMAX port n = do
    setInputModeConfirm Three LightActive PctFullScaleMode
    lst <- replicateM n (getInputValues port)
    let threshold = (foldr1 max (map getScaledValue lst))
    return threshold


calibrateAVG :: InputPort -> Int -> NXT ScaledValue
calibrateAVG port n = do
    lst <- replicateM n (readSensor port LightActive PctFullScaleMode)
    let threshold = (foldr1 (+) lst) `div` n
    return threshold


rotateMotor :: OutputPort -> OutputPower -> NXT ()
rotateMotor port power =
    setOutputStateConfirm port power modes regMode 0 motorRunState 0
  where modes = [MotorOn, Brake,Regulated]
        regMode = RegulationModeIdle 
        motorRunState = MotorRunStateRunning


-- | A getter for the sensor reading of interest.
getScaledValue :: InputValue -> ScaledValue
getScaledValue (InputValue _ _ _ _ _ _ _ x _) = x


readSensor :: InputPort -> SensorType -> SensorMode -> NXT ScaledValue
readSensor port sType sMode = do
  setInputModeConfirm port sType sMode
  reading <- fmap getScaledValue $ getInputValues port
  resetInputScaledValue port
  return reading


maybeTurn :: Logger -> Int -> Int -> NXT ()
maybeTurn logger threshold reading
  | reading == threshold = do
      liftIO $ writeTo logger Debug $ show reading ++ " = " ++ show threshold
      liftIO $ writeTo logger Debug $ "--> Keep Straight"
      rotateMotor A 25
      rotateMotor B 25
  | reading > threshold = do
      liftIO $ writeTo logger Debug $ show reading ++ " > " ++ show threshold
      liftIO $ writeTo logger Debug $ "--> Turning Left"
      rotateMotor A 25
      rotateMotor B 0
  | reading < threshold = do
      liftIO $ writeTo logger Debug $ show reading ++ " < " ++ show threshold
      liftIO $ writeTo logger Debug $ "--> Turning Right"
      rotateMotor A 0
      rotateMotor B 25
  | otherwise = return ()


followLineWith :: Logger -> ScaledValue -> NXT ()
followLineWith logger lineValue = do
    reading <- calibrateAVG Three 5
    liftIO $ writeTo logger Debug $ "Reading is: " ++ show reading
    maybeTurn logger lineValue reading
    rotateMotor A 25
    rotateMotor B 25
