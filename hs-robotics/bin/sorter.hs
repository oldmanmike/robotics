import Control.Monad
import Data.List
import Debug.Trace
import Robotics.NXT

-- | Synonym for the default parameters passed to the motor function.
rotateMotor :: OutputPort -> OutputPower -> NXT ()
rotateMotor port power =
    setOutputStateConfirm port power modes regMode 0 motorRunState 0
  where modes = [MotorOn, Brake,Regulated]
        regMode = RegulationModeIdle 
        motorRunState = MotorRunStateRunning

-- | A getter for the sensor reading of interest.
getScaledValue :: InputValue -> ScaledValue
getScaledValue (InputValue _ _ _ _ _ _ _ x _) = x

-- | Takes a reading by the sensor and chooses whether to move motor A 
-- either clockwise, counter-clockwise, or none at all.
sorter :: ScaledValue -> NXT ()
sorter ambientThreshold = do
    setInputModeConfirm Two Switch BooleanMode
    setInputModeConfirm Three LightActive PctFullScaleMode
    v <- getInputValues Three
    let colorValue = getScaledValue v
    traceM $ "Reading is: " ++ show colorValue
    if colorValue > ambientThreshold
        then if colorValue >= 50 -- 47
                then rotateMotor A 75 >> stopEverything
                else rotateMotor A (-75) >> stopEverything
        else stopEverything
    stopEverything

-- | Takes n readings of the environment with the light sensor and returns 
-- the largest reading of them all. Used to set a good threshold for the 
-- sensor later on to determine whether there's a brick in front of it in 
-- the first place.
calibrate :: Int -> NXT ScaledValue
calibrate n = do
    setInputModeConfirm Three LightActive PctFullScaleMode
    lst <- replicateM n (getInputValues Three)
    traceM $ "Readings are: " ++ show lst
    let threshold = (foldr1 max (map getScaledValue lst))
    traceM $ "Threshold is: " ++ show threshold
    return threshold

main :: IO ()
main = do
    withNXT defaultDevice $ do 
        threshold <- calibrate 500
        forever (sorter threshold)
    return ()
