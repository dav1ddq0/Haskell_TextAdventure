module MainGame where

import System.IO
import System.Exit
import GameModeling
import GameLexer
import GameData
import GameParser
import GameUtils
import Control.Monad
import GameProcessing

-- Parse input and obtein possible sentences from it
readInput :: [Char] -> IO (Maybe [Sentence])
readInput  text = do
        hFlush stdout
        let tokens = textSplit text
        let sentenceTokenMatches = lexInput worldWords  tokens
        let sentences = parseSentence sentenceTokenMatches
        if null sentences 
                then do
                        putStr "I do not understand.\nWhat are you wanting to tell me" 
                        hFlush stdout 
                        return (Just sentences)
        else
                return (Just sentences)



gameLoop :: World -> String -> Maybe [Sentence] -> IO (Maybe (World, String))
gameLoop _ _ Nothing = return Nothing 
gameLoop world locationId  (Just []) = goToAdventure (Just (world, locationId))
gameLoop world locationId  (Just sentences) = do
                resultP <-performInteraction  world locationId  sentences 
                goToAdventure resultP 




updateAdventure :: Maybe (World, String) -> IO (Maybe (World, String))
updateAdventure Nothing = return Nothing
updateAdventure (Just (world, locationId))=do 
        putStr "\n> " 
        hFlush stdout
        -- printInvalidInteractions world locationId >>
        line <- getLine 
        readInput  line >>= (\state -> putStr "\n" >> hFlush stdout >> return state) 
        >>= gameLoop world locationId 




goToAdventure :: Maybe  (World, String) -> IO (Maybe  (World, String))
goToAdventure Nothing = putStr "Thanks for playing" >> hFlush stdout >> return Nothing
goToAdventure (Just (world,locationId))  = updateAdventure (Just (world,locationId))



-- Play the Game :)
-- --------------------------------------------------------------

play :: IO ()
play = do
        player <- buildPlayer
        putStrLn intro 
        hFlush stdout 
        goToAdventure (Just (gameWorld player,startLocation)) 
        return ()
