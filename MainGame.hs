module MainGame where

import System.IO
import System.Exit
import GameModeling
import GameLexer
import GameData
import GameParser
import Data.List.Split
import Control.Monad
import GameProcessing
textSplit :: [Char] -> [[Char]]
textSplit = splitOneOf [' ', '\t']  


readInput :: [Char] -> IO (Maybe [Sentence])
readInput  text = do
        hFlush stdout
        let tokens = textSplit text
        let sentenceTokenMatches = lexInput worldWords  tokens
        let sentences = parseSentence sentenceTokenMatches
        if null sentences 
                then putStr "You have not entered valid text. Please write a valid text\n\n" >> hFlush stdout >> return (Just sentences)
        else
                return (Just sentences)



gameLoop :: World -> String -> Maybe [Sentence] -> IO (Maybe (World, String))
gameLoop _ _ Nothing = return Nothing 
gameLoop world roomId  (Just []) = goToAdventure (Just (world, roomId))
gameLoop world roomId  (Just sentences) = do
                resultP <-performInteraction  world roomId  sentences 
                goToAdventure resultP 




updateAdventure :: Maybe (World, String) -> IO (Maybe (World, String))
updateAdventure Nothing = return Nothing
updateAdventure (Just (world, roomId))
        = putStr "\n> " >> hFlush stdout >>
        -- printInvalidInteractions world roomId >>
        getLine >>= 
        readInput  >>=
        (\state -> putStr "\n" >> hFlush stdout >> return state) >>=
        gameLoop world roomId 




goToAdventure :: Maybe  (World, String) -> IO (Maybe  (World, String))
goToAdventure Nothing = putStr "Thanks for playing" >> hFlush stdout >> return Nothing
goToAdventure (Just (world,roomId))  = updateAdventure (Just (world,roomId))




gameWorld :: Player -> World
gameWorld player = createWorld  rooms ends  commonActions player     
        where (rooms, ends) = locationsMap

-- gameStartMaybe :: Maybe ([Char], Inventory, Labels)
-- gameStartMaybe = Just (startRoom , startInventory, startTags  )




play :: IO ()
play = do
        player <- buildPlayer
        putStrLn intro 
        hFlush stdout 
        goToAdventure (Just (gameWorld player,startLocation)) 
        return ()
