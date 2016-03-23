import Control.Monad
import Robotics.NXT
import Robotics.NXT.Sensor.Compass
import Debug.Trace


compass :: NXT ()
compass = do
  csInit Three
  m <- csGetMeasurement Three
  traceM $ "Compass Reading: " ++ show m
  return ()

main :: IO ()
main = do
  withNXT defaultDevice $ do
        forever compass
  return ()
