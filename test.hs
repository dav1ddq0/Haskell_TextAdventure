import Data.Time
import Data.Time.Clock
import Data.Time.LocalTime

gameTime :: IO ()
gameTime= do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let (TimeOfDay hour minute second) = localTimeOfDay $ utcToLocalTime timezone now
    putStrLn $ "Time: " ++ getAMPM hour (getAddZero minute) 

getAddZero :: (Ord a, Num a, Show a) => a -> String
getAddZero min | min < 10 = "0" ++ show min
                |otherwise  = show min 

getAMPM :: (Ord a, Num a, Show a) => a -> [Char] -> [Char]
getAMPM hour min | hour > 12 = show (hour - 12) ++ ":" ++  min ++ " PM"
                | otherwise  = show hour  ++ ":" ++ min ++ " AM"

getTimeUI =  Data.Time.getCurrentTime 


