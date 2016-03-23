import Control.Monad
import Data.List
import Debug.Trace
import Robotics.NXT
import Bot.Logic

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


main :: IO ()
main = do
    withNXT defaultDevice $ do 
        threshold <- calibrate Three 500
        forever (sorter threshold)
    return ()
