module GameModeling where
import GameParser
import Data.List
import qualified Data.List
import qualified Data.Map
import System.IO
import GameUtils

type Tag = [String]
type Bag = [String]
data GameAction     =  AddItemToBag String|
                    RemoveItemFromBag String |
                    PrintBag |
                    AddTag String|
                    RemoveTag String|
                    GTime |
                    Hit String|
                    PhysicAttack String|
                    MagicAttack String|
                    RoomChange String deriving (Eq, Show)

data GameCondition = YouAlreadyHaveThisItem String |
                    ThisLocation String|
                    TagExist String|
                    GameTrue |
                    GameFalse |
                    GameNot GameCondition|
                    GameOr GameCondition GameCondition|
                    GameAnd GameCondition GameCondition deriving (Eq, Show)

data Player = Player {
    playerName ::String,
    playerLife :: Int,
    playerMagic :: Int,
    bag:: Bag

}deriving(Show,Eq)



data InteractionAction = InteractionAction{
    actionCondition :: GameCondition,
    actionDescription :: String,
    actionGameActions :: [GameAction]
}deriving (Eq, Show)

data LocationInteraction = LocationInteraction {
    interactionSentences :: [Sentence],
    interactionActions :: [InteractionAction] 
}deriving (Show,Eq)

data Location = Location {
    locationId :: String,
    locationDescription :: String,
    locationInteractions :: [LocationInteraction]
} deriving(Show,Eq)


data World = World{
    locations :: Data.Map.Map String Location,
    player :: Player,
    tags :: Tag,
    endGames :: [String],
    communActions :: Location
}deriving (Show, Eq)






-- printInventory :: Inventory -> IO ()

                            



-- newInvRemoveItem :: Inventory -> String -> Inventory
-- newInvRemoveItem(Inventory items) item = Inventory (filter (/= item)  items)


-- printRoomDescription :: World-> Maybe ([Char], Inventory, Labels)-> IO (Maybe (String, Inventory, Labels))
-- printRoomDescription World{rooms = gameRooms, endGames = endScenes} Nothing
--     = return Nothing


-- printRoomDescription World{rooms = gameRooms, endGames = endScenes} (Just (roomId, inventory,flags))
--     = let room = searchInDicc roomId gameRooms
--         in case room of
--             Nothing -> putStrLn (roomId ++ "Not valid room\n") >> return Nothing
--             Just Room {roomDescription = description}
--                 -> printConditionalDescription  endScenes description [] (Just (roomId, inventory, flags))


-- updateGameState :: [String]-> String-> Inventory-> Labels-> ConditionalAction-> IO (Maybe (String, Inventory, Labels))
-- updateGameState endGames roomId inventory labels conditionalAction@(
--     ConditionalAction {actionDescription = description, actionStateChanges = stateChanges})
--     = printConditionalDescription endGames description [] (Just (roomId, inventory, labels)) >>=
--         stateChange (Data.List.find (\x -> case x of 
--                                             (RoomChange _) -> True
--                                             otherwise -> False) stateChanges)
--                     endGames
--                     stateChanges


-- filterInteraction Room {roomInteractions = thisRoomInteraction} Room {roomInteractions = defaultRoomInteractions}
--                 roomId  sentences
--             = applyConditionalActions roomId endGames inventory flags interaction defaultInteraction
--                 where (interaction, defaultInteraction) = (interactionSearch thisRoomInteraction sentences,interactionSearch defaultRoomInteractions sentences)


-- printText :: [String] -> IO ()
-- printText [] = putStr ""
-- printText (str:xs) = printText xs >> putStr str

-- printConditionalDescription :: [String] -> ConditionalDescription -> [String] -> Maybe (String, Inventory, Labels) -> IO (Maybe (String, Inventory, Labels))
-- printConditionalDescription _ (ConditionalDescription []) lines Nothing
--     = printText lines >> putStr "\n" >> return Nothing -- Se acabo el juego


-- printConditionalDescription _ (ConditionalDescription []) lines (Just (roomId, inventory, flags))
--     = printText lines >> putStr "\n" >>hFlush stdout >> return (Just (roomId, inventory, flags) )

-- printConditionalDescription  _ (ConditionalDescription ((_ ,_ ,_ ):otherDescriptions)) lines Nothing
--     = printText lines >> putStr "\n" >> return Nothing -- Se acabo el juego

-- printConditionalDescription  endGames (ConditionalDescription ((condition , description ,stateChanges):otherDescriptions))
--         lines  (Just (roomId, inventory, flags))
--     | evalCondition condition roomId inventory flags 
--         = stateChange (Data.List.find (\x -> case x of
--                                         (RoomChange _) -> True
--                                         otherwise ->  False) stateChanges)
--             endGames
--             stateChanges
--             (Just (roomId, inventory, flags)) >>=
--                 printConditionalDescription endGames (ConditionalDescription otherDescriptions) ((description ++ " ") : lines) 
--     | otherwise
--         = printConditionalDescription endGames (ConditionalDescription otherDescriptions) lines (Just (roomId, inventory, flags))




-- findAllInteractions :: (Foldable t, Applicative t) =>t RoomInteraction -> t Sentence -> Maybe RoomInteraction
-- findAllInteractions interactions sentences = 
--     find matchInteraction (getPairs interactions sentences) >>= (\(x,y)-> Just x)


-- applyConditionalActions :: [Char]-> [String]-> Inventory-> Labels-> Maybe RoomInteraction-> Maybe RoomInteraction-> IO (Maybe ([Char], Inventory, Labels))
-- applyConditionalActions roomId _ inventory flags Nothing Nothing
--     = return (Just (roomId, inventory, flags))

-- applyConditionalActions roomId endGames inventory flags (Just RoomInteraction {sentences = _, actions =[]}) defaultRoomInteractions
--     = applyConditionalActions roomId endGames inventory flags Nothing defaultRoomInteractions

-- applyConditionalActions roomId endGames inventory flags 
--     (Just RoomInteraction {sentences = thisSentences,
--     actions = (conditionalAction@(ConditionalAction {actionCondition = thisCondition, actionStateChanges = thisStates}) : remainingConditionalActions)}) defaultRoomInteractions
--     | evalCondition thisCondition roomId inventory flags = do
--         result <- updateGameState endGames roomId inventory flags conditionalAction
--         if result == Nothing 
--             then return Nothing 
--         else
--             do
--             newRoomId <- getRoomId result
--             newInv <- getInv result
--             newLabels <- getTags result
--             applyConditionalActions newRoomId endGames newInv newLabels ( Just (RoomInteraction {sentences = thisSentences,
--                                                                 actions = remainingConditionalActions})) defaultRoomInteractions

--     | otherwise = applyConditionalActions roomId endGames inventory flags ( Just (RoomInteraction {sentences = thisSentences,
--                                                                 actions = remainingConditionalActions})) defaultRoomInteractions

-- applyConditionalActions roomId endGames inventory flags
--     Nothing (Just RoomInteraction {actions = []})
--     = applyConditionalActions roomId endGames inventory flags Nothing Nothing




-- applyConditionalActions roomId endGames inventory flags 
--     Nothing (Just (RoomInteraction{sentences = thisSentences, actions = (conditionalAction@(ConditionalAction {actionCondition = thisCondition}):remainingConditionalActions)}))
--     | evalCondition thisCondition roomId inventory flags = do
--         result <- updateGameState endGames roomId inventory flags conditionalAction
--         if result == Nothing 
--             then return Nothing 
--         else
--             do
--             newRoomId <- getRoomId result
--             newInv <- getInv result
--             newLabels <- getTags result
--             applyConditionalActions newRoomId endGames newInv newLabels Nothing (Just (RoomInteraction {sentences = thisSentences, actions = remainingConditionalActions}))  

--     | otherwise = applyConditionalActions roomId endGames inventory flags Nothing
--                     (Just (RoomInteraction {sentences = thisSentences, actions = remainingConditionalActions}))


getRoomId update = case update of
        Nothing ->  return ""
        Just (roomId, inventory, tags) -> return roomId


-- getInv :: Monad m => Maybe (a, Inventory, c) -> m Inventory
-- getInv update = case update of
--         Nothing ->  return (Inventory [])
--         Just (roomId, inventory, tags) -> return inventory


-- getTags :: Monad m => Maybe (a, b, Labels) -> m Labels
-- getTags update = case update of
--         Nothing ->  return (Labels [])
--         Just (roomId, inventory, tags) -> return tags

-- -- newInvRemoveItem :: Inventory -> String -> Inventory
-- newInvRemoveItem(Inventory items) item = Inventory (filter (/= item)  items)

-- newInvAddItem :: Inventory -> String -> Inventory
-- newInvAddItem (Inventory items) newItem 
--     | newItem `elem` items = Inventory items
--     | otherwise = Inventory (items ++ [newItem])
-- Actualizar el inventario de objetos del jugador

-- updateInventory (Inventory inventory) [] = Inventory inventory

-- updateInventory currentInventory@(Inventory inventory) ((RemoveItemToInventory item) : otherChanges)
--     = updateInventory (newInvRemoveItem currentInventory item ) otherChanges

-- updateInventory currentInventory@(Inventory inventory) ((AddItemToInventory item) : otherChanges)
--     = updateInventory (newInvAddItem currentInventory item ) otherChanges

-- updateInventory currentInventory@(Inventory inventory) (_ : otherChanges) 
--     = updateInventory currentInventory otherChanges

-----------------------------------------------------------------------------------------
-- newLabelsDel :: Labels -> String -> Labels
-- newLabelsDel (Labels labels) label = Labels (filter (/= label) labels)

-- newLabelsAdd :: Labels -> String -> Labels
-- newLabelsAdd (Labels labels) newLabel
--     | newLabel `elem` labels = Labels labels
--     | otherwise = Labels (labels ++ [newLabel])


-- updateLabels currentLabels@(Labels labels) [] = currentLabels
-- updateLabels currentLabels@(Labels labels) ((RemoveTag label) : otherChanges)
--     = updateLabels (newLabelsDel currentLabels label) otherChanges

-- updateLabels currentLabels@(Labels labels) ((AddTag label) : otherChanges )
--     = updateLabels (newLabelsAdd currentLabels label) otherChanges

-- updateLabels currentLabels@(Labels labels) (_ : otherChanges )
--     = updateLabels currentLabels otherChanges

-- stateChange :: Maybe  StateChange -> [String] -> [StateChange] -> Maybe (String, Inventory, Labels) -> IO (Maybe (String, Inventory, Labels))
-- stateChange Nothing _ stateChanges Nothing
--     = return Nothing

-- stateChange _ endGames stateChanges Nothing
--     = return Nothing

-- stateChange Nothing _ stateChanges (Just (roomId, inventory, labels))
--     = do
--         printInventoryFromStates stateChanges inventory
--         printTimeFromStates stateChanges
--         return (Just (roomId, updateInventory inventory stateChanges,updateLabels labels stateChanges))


-- stateChange (Just (RoomChange nextRoom)) endGames stateChanges (Just (roomId, inventory, labels))
--     = if nextRoom `elem` endGames
--         then putStr "GameFinished" >> return Nothing 
--     else
--         do
--         printTimeFromStates stateChanges
--         printInventoryFromStates stateChanges inventory
--         return (Just (nextRoom, updateInventory inventory stateChanges, updateLabels labels stateChanges))



-- -- For exhaustic Pattern Matching
-- stateChange _ _ _ _ = return Nothing


-- printInventoryFromStates [] inventory =  putStr ""
-- printInventoryFromStates (PrintInventory : otherchanges) inventory =
--     printInvantoryMain inventory >> printInventoryFromStates otherchanges inventory
-- printInventoryFromStates (_:otherchanges) inventory=
--     printInventoryFromStates otherchanges inventory


-- printTimeFromStates [] = putStr ""
-- printTimeFromStates (GTime:otherchanges) = getGameTime >> printTimeFromStates otherchanges
-- printTimeFromStates (_:otherchanges) = printTimeFromStates otherchanges

getRoomFromId :: World -> String -> Maybe Location
getRoomFromId World{locations = gameLocations} roomId = searchInDicc roomId gameLocations



-- executeActions [] world room roomId =
--     return Just (world, roomId)
-- executeActions (action@ConditionalAction{actionDescription = thisDescription, gameAction = thisGameActions})
--     world room roomId
--     = do 
--         putStrLn thisDescription
        

-- printStageDes :: World -> String -> Maybe Room
-- printStageDes World{rooms=thisRoom} roomId= do
--     let room = roomsearchInDicc roomId thisRoom

-- hasInvalidInteractions :: [RoomInteraction] -> Maybe RoomInteraction
-- hasInvalidInteractions [] = Nothing
-- hasInvalidInteractions (interaction@(RoomInteraction {sentences = thisSentences}) : remainingInteractions)
--     | NullSentence `elem` thisSentences = Just interaction
--     | otherwise = hasInvalidInteractions remainingInteractions

-- printInvalidInteractions :: World -> String -> IO ()
-- printInvalidInteractions world@(World {rooms = thisRooms}) roomId
--     = let room = searchInDicc roomId thisRooms 
--         in case room of
--         Nothing -> putStrLn (roomId ++ " is not a valid scene") >> return ()
--         Just Room {roomInteractions = thisInteractions}
--             -> case hasInvalidInteractions thisInteractions of
--                     Nothing -> return ()
--                     Just interaction@(RoomInteraction {sentences = thisSentences}) -> putStrLn ("Invalid interaction: " ++  (show interaction))


