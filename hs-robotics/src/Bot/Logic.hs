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
import System.Exit

data Cont = KeepGoing | PleaseStop deriving (Show,Eq)

data LineState
  = LineCenter
  | LineRight
  | LineLeft
  | LineLost
  deriving (Show,Eq)

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


getScaledValue :: InputValue -> ScaledValue
getScaledValue (InputValue _ _ _ _ _ _ _ x _) = x


readSensor :: InputPort -> SensorType -> SensorMode -> NXT ScaledValue
readSensor port sType sMode = do
  setInputModeConfirm port sType sMode
  reading <- fmap getScaledValue $ getInputValues port
  resetInputScaledValue port
  return reading


maybeTurn :: Logger -> Int -> Int -> NXT ()
maybeTurn logger readingLeft readingRight
  | (readingLeft >= 60) || (readingRight >= 60) = do
      liftIO $ writeTo logger Debug $ "Stopping"
      rotateMotor A (30)
      rotateMotor B (30)
      stopEverything
      shutdown
      liftIO exitSuccess
      return ()
  | (readingLeft <= 43) || (readingRight <= 43) = do
      rotateMotor A 20
      rotateMotor B 20
      carefulFollow logger
  | abs (readingLeft - readingRight) <= 2 = do
      rotateMotor A 30
      rotateMotor B 30
      return ()
  | (abs (readingLeft - readingRight) > 2) && (not (abs (readingLeft - readingRight) > 12)) = do
    if readingLeft > readingRight
      then do
        -- Right side is detecting the line
        liftIO $ writeTo logger Debug $ "--> Turn Right"
        rotateMotor A 45
        rotateMotor B 15
        return ()
      else do
        -- Left side is detecting the line
        liftIO $ writeTo logger Debug $ "--> Turn Left"
        rotateMotor A 15
        rotateMotor B 45
        return ()
  | otherwise = return ()


carefulTurn :: Logger -> Int -> Int -> NXT ()
carefulTurn logger readingLeft readingRight
  | (readingLeft >= 60) || (readingRight >= 60) = do
      liftIO $ writeTo logger Debug $ "Stopping"
      rotateMotor A (30)
      rotateMotor B (30)
      stopEverything
      shutdown
      liftIO exitSuccess
      return ()
      {-
  | abs (readingLeft - readingRight) <= 2 = do
      liftIO $ writeTo logger Debug $ "--> Careful Go Straight"
      rotateMotor A 20
      rotateMotor B 20
      return ()
      -}
  | (abs (readingLeft - readingRight) > 2) = do
    if readingLeft > readingRight
      then do
        -- Right side is detecting the line
        liftIO $ writeTo logger Debug $ "--> Careful Turn Right"
        rotateMotor A 25
        rotateMotor B 15
        return ()
      else do
        -- Left side is detecting the line
        liftIO $ writeTo logger Debug $ "--> Careful Turn Left"
        rotateMotor A 15
        rotateMotor B 25
        return ()
  | otherwise = return ()

carefulFollow :: Logger -> NXT ()
carefulFollow logger = do
    reading1 <- calibrateAVG One 3
    reading2 <- calibrateAVG Two 3
    liftIO $ writeTo logger Debug $ "Right Reading is: " ++ show reading1
    liftIO $ writeTo logger Debug $ "Left Reading is: " ++ show reading2
    liftIO $ writeTo logger Debug $ "Difference is: " ++ show (abs (reading1 - reading2)) 
    maybeTurn logger reading1 reading2
    if (not (reading1 <= 43) || (reading1 <= 43))
      then return ()
      else carefulFollow logger
    

followLineWith :: Logger -> NXT ()
followLineWith logger = do
    reading1 <- calibrateAVG One 1
    reading2 <- calibrateAVG Two 1
    liftIO $ writeTo logger Debug $ "Right Reading is: " ++ show reading1
    liftIO $ writeTo logger Debug $ "Left Reading is: " ++ show reading2
    liftIO $ writeTo logger Debug $ "Difference is: " ++ show (abs (reading1 - reading2)) 
    maybeTurn logger reading1 reading2
