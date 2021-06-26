module GameUtils where
import Data.Map

import Data.Time
import Data.Time.Clock
import Data.Time.LocalTime
import Data.List.Split
-- devuele la hora actual de la pc en la que se este corriendo el juego
-------------------------------------------------------------------------------------
getGameTime :: IO ()
getGameTime= do
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
--------------------------------------------------------------------------------------
-- 
searchInDicc :: Ord k => k -> Map k a -> Maybe a
searchInDicc = Data.Map.lookup 
--
getPairs :: Applicative f => f a1 -> f a2 -> f (a1, a2)
getPairs a b = (,) <$> a <*> b

-- -----------------------------------------------------------------------------------

textSplit :: [Char] -> [[Char]]
textSplit = splitOneOf [' ', '\t']  
-- -----------------------------------------------------------------------------------
