module GameProcessing where

import GameModeling
import GameParser
import GameLexer
import GameUtils
import System.IO
import Data.List
import qualified Data.List
import qualified Data.Map

-- Evaluar la condicion para saber si se cumple la interaccion
-- -------------------------------------------------------------------------------
evalCondition :: GameCondition -> World -> Location-> Bool
evalCondition GameTrue _ _  = True 
evalCondition GameFalse _ _  = False 
evalCondition (YouAlreadyHaveThisItem item)  
    World { player = Player{bag = playerBag}} _
    = item `elem` playerBag

evalCondition (TagExist tag) World {tags = gameTags} _
    = tag `elem` gameTags

evalCondition (ThisLocation locationId) _ Location{locationId =thisId} 
    = locationId == thisId

evalCondition (GameNot condition) world room 
    = not (evalCondition condition world room) 
evalCondition (GameOr condition1 condition2) world room 
    = evalCondition condition1 world room || evalCondition condition2 world room
evalCondition (GameAnd condition1 condition2) world room 
    = evalCondition condition1 world room && evalCondition condition2 world room

-- search interaction with matched with current sentence
-- -------------------------------------------------------------------------------
matchSInteraction :: (LocationInteraction, Sentence) -> Bool
matchSInteraction (LocationInteraction {interactionSentences = thisInteractionSentences}, sentence)
    | sentence `elem` thisInteractionSentences = True
    | otherwise = False

searchSInteraction :: [LocationInteraction] -> [Sentence] -> Maybe LocationInteraction
searchSInteraction interactions sentences = 
    find matchSInteraction (getPairs interactions sentences) 
    >>= (\(x,y)-> Just x) -- lambda function to obtein Maybe result with only x


-- --------------------------------------------------------------------------------

-- Print the description of the stage
printRoomDescription :: Maybe Location-> IO ()
printRoomDescription Nothing = putStr ""
printRoomDescription (Just Location{locationDescription=thisDescription}) =
    putStr thisDescription


-- Execute  each action given a list of actions
-- ---------------------------------------------------------------------------------
exeStageAction :: [InteractionAction]-> World -> [Char] -> IO (Maybe (World, [Char]))
exeStageAction [] world roomId =
    return (Just(world,roomId))
    
exeStageAction (InteractionAction{
    actionDescription = thisDescription,
    actionGameActions = thisGameActions}:otherStageActions) 
    world roomId
    = do
        printDescription thisDescription
        outEx <-exeGameAction thisGameActions world roomId
        newRoomId <- getRoomIdFromMaybeT outEx
        newWorld <- getWorldFromMaybe outEx
        exeStageAction otherStageActions newWorld newRoomId
        
-- ---------------------------------------------------------------------------------



-- ----------------------------------------------------------------------------------
--Bag Possible Actions
-- Add a new item to player bag
addItemToBagAction :: Eq a => [a] -> a -> [a]
addItemToBagAction items newItem
    | newItem `elem` items = items
    | otherwise = items ++ [newItem]

-- Remove item from the player inventory
removeItemFromBagAction :: Eq a => [a] -> a -> [a]
removeItemFromBagAction items itemToRemove
    = filter (/= itemToRemove)  items

-- Print current player inventory

printBag :: Bag -> IO ()
printBag [] = do 
                        putStr "\n" 
                        hFlush stdout

printBag (item:restItems) = do
                                    putStr "*"
                                    putStrLn  item 
                                    printBag restItems
                                            

printBagMain :: [String] -> String -> IO ()
printBagMain bag name = do 
                            putStrLn (name ++ "Bag:\n") 
                            printBag bag  

-- ----------------------------------------------------------------------------------------



exeGameAction :: [GameAction] -> World -> String -> IO (Maybe (World, String))
exeGameAction [] world roomId  = 
    return (Just(world, roomId))

exeGameAction ((AddItemToBag item) : otherGameAction) 
    World{locations = thisStages,
        player = Player{
            playerName = thisPlayerName,
            playerLife = thisPlayerLife,
            playerMagic = thisPlayerMagic,
            bag = thisBag},
        tags = thisTags,
        endGames =thisEnds,
        communActions = thisDefault}
        roomId
    = do 
        let newBag =  addItemToBagAction thisBag item
        exeGameAction otherGameAction (World{
            locations = thisStages,
            player = (Player{playerName = thisPlayerName,playerLife = thisPlayerLife,playerMagic = thisPlayerMagic,bag = newBag}),
            tags = thisTags,
            endGames = thisEnds,
            communActions = thisDefault}) roomId

exeGameAction ((RemoveItemFromBag item) : otherGameAction) 
    World{locations = thisRooms,
        player = Player{
            playerName = thisPlayerName,
            playerLife = thisPlayerLife,
            playerMagic = thisPlayerMagic,
            bag = thisInventory},
        tags = thisTags,
        endGames =thisEnds,
        communActions = thisDefault}
        roomId
    = do 
        let newBag =  removeItemFromBagAction thisInventory item
        exeGameAction otherGameAction (World{
            locations = thisRooms,
            player = (Player{playerName = thisPlayerName,playerLife = thisPlayerLife,playerMagic = thisPlayerMagic,bag= newBag}),
            tags = thisTags,
            endGames = thisEnds,
            communActions = thisDefault}) roomId

exeGameAction (PrintBag  : otherGameAction) 
    world@World{locations = thisRooms,
        player = Player{
            playerName = thisPlayerName,
            playerLife = thisPlayerLife,
            playerMagic = thisPlayerMagic,
            bag = thisBag},
        tags = thisTags,
        endGames =thisEnds,
        communActions = thisDefault}
        roomId
    = do 
        printBagMain thisBag thisPlayerName
        return (Just(world, roomId))

-- exhaustic pattern mathing
exeGameAction _ _ _ = return Nothing 




    
printDescription :: String -> IO ()
printDescription description =
    putStrLn description


getWorldFromMaybe :: Monad m => Maybe (World, b) -> m World
getWorldFromMaybe input = case input of
    Nothing -> return World{}
    Just (world, roomId) -> return world

getRoomIdFromMaybeT :: Monad m => Maybe (a, [Char]) -> m [Char]
getRoomIdFromMaybeT input = case input of
    Nothing -> return ""
    Just (world, roomId) -> return roomId




getNotMaybeRoomId room = case room of
    Nothing -> return Location{locationId ="", locationDescription ="", locationInteractions = []}
    Just word -> return word

getNotMaybeRoomIntegration :: Monad m => Maybe LocationInteraction -> m LocationInteraction
getNotMaybeRoomIntegration roomInt = case roomInt of
        Nothing ->  return LocationInteraction{interactionSentences=[], interactionActions= []}
        Just interaction@LocationInteraction{interactionSentences=_, interactionActions =_} -> return interaction


getValidConditionalActions :: World -> Location-> LocationInteraction -> [InteractionAction]
getValidConditionalActions world room LocationInteraction{interactionActions = []}
    = []

getValidConditionalActions world room  LocationInteraction{interactionSentences = thisSentences, interactionActions = (thisAction@(InteractionAction{actionCondition = thisCondition}):otherActions)}
        | evalCondition thisCondition world room =  thisAction : getValidConditionalActions world room LocationInteraction{interactionSentences = thisSentences, interactionActions = otherActions} 
        | otherwise = getValidConditionalActions world room LocationInteraction{interactionSentences = thisSentences,interactionActions = otherActions}

getMatchedInteractions :: Monad m => World -> Location-> [Sentence] -> m (Maybe LocationInteraction)
getMatchedInteractions  World {communActions = Location{locationInteractions=communGameInteractions}} 
                        Location{locationInteractions  = thisLocationInteraction}
                        sentences
                    =  do
                        let maybeInteractionFromThisRoom = searchSInteraction thisLocationInteraction sentences
                        let maybeInteractionFromCommun   = searchSInteraction communGameInteractions sentences
                        if maybeInteractionFromCommun == Nothing 
                            then return maybeInteractionFromThisRoom
                        else
                            return maybeInteractionFromCommun




performInteraction world roomId  []
    = putStrLn "Please enter a command.">>
    hFlush stdout>>
    return (Just (world, roomId))


performInteraction world roomId  sentences
    = let room = getRoomFromId world roomId
        in case room of
            Nothing -> hFlush stdout >> putStrLn (roomId ++ "is not a valid room") >> return Nothing 
            Just newRoom -> hFlush stdout >>
                do 
                interactions  <- getMatchedInteractions world newRoom sentences

                if interactions == Nothing  then return (Just (world, roomId))
                else
                    do
                        printRoomDescription room
                        pureInteraction <- getNotMaybeRoomIntegration interactions
                        pureNotMaybeRoom <- getNotMaybeRoomId room
                        let actions = getValidConditionalActions world pureNotMaybeRoom pureInteraction
                        exeStageAction actions world roomId 
