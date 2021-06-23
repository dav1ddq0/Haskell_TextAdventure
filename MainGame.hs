module MainGame where

import System.IO
import System.Exit
import GameModeling
import GameLexer
import GameData
import GameParser
import Data.List.Split
import Control.Monad

textSplit :: [Char] -> [[Char]]
textSplit = Data.List.Split.splitOneOf [' ', '\t']  

readInput :: p1 -> p2 -> [Char] -> IO (Maybe [Sentence])
readInput inventory flags text = do
        hFlush stdout
        let tokens = textSplit text
        let sentenceTokenMatches = lexInput worldWords  tokens
        let sentences = parseSentence sentenceTokenMatches
        if null sentences 
                then putStr "You have not entered valid text. Please write a valid text\n\n" >> hFlush stdout >> return (Just sentences)
        else
                return (Just sentences)

gameLoop :: World -> String -> Inventory -> Labels -> Maybe [Sentence] -> IO (Maybe (String, Inventory, Labels ))
gameLoop _ _ _ _ Nothing = return Nothing 
gameLoop world roomId inventory labels (Just []) = adventure world (Just (roomId, inventory, labels)) 
gameLoop world roomId inventory labels (Just sentences) = do
                performInteraction  world roomId inventory labels sentences >>= adventure world  

updateAdventure :: World -> Maybe (String, Inventory, Labels) -> IO (Maybe (String, Inventory, Labels ))
updateAdventure _ Nothing = return Nothing
updateAdventure world (Just (roomId, inventory, labels))
        = putStr "\n> " >> hFlush stdout >>
        printInvalidInteractions world roomId >>
        getLine >>= 
        readInput inventory labels >>=
        (\state -> putStr "\n" >> hFlush stdout >> return state) >>=
        gameLoop world roomId inventory labels


adventure :: World -> Maybe (String, Inventory, Labels) -> IO (Maybe (String, Inventory, Labels))
adventure  _ Nothing = putStr "Thanks for playing" >> hFlush stdout >> return Nothing
adventure world (Just (roomId, inventory, labels)) = 
        printRoomDescription  world (Just (roomId, inventory, labels)) 
        >>= updateAdventure world




gameWorld :: World
gameWorld = createWorld  rooms ends  communRoom 
        where (rooms, ends) = roomsMap

gameStartMaybe :: Maybe ([Char], Inventory, Labels)
gameStartMaybe = Just (startRoom , startInventory, startLabels )


play :: IO ()
play = do
        putStrLn intro 
        hFlush stdout 
        adventure gameWorld gameStartMaybe
        return ()
