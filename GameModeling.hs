module GameModeling where
import GameParser
import Data.List
import qualified Data.List
import qualified Data.Map
import System.IO

newtype Inventory = Inventory [String] deriving (Show, Eq)
type SceneIndex = Int
newtype Labels = Labels [String] deriving (Show, Eq)

data StateChange =  AddItemToInventory String|
                    RemoveItemToInventory String |
                    PrintInventory |
                    AddTag String|
                    RemoveTag String|
                    Hit String|
                    PhysicAttack String|
                    RoomChange String deriving (Eq, Show)

data GameCondition = YouAlreadyHaveThisItem String |
                    ThisRoom String|
                    LabelExist String|
                    CTrue |
                    CFalse |
                    CNot GameCondition|
                    COr GameCondition GameCondition|
                    CAnd GameCondition GameCondition deriving (Eq, Show)

data Character = Character {
    characterName ::String,
    characterLife :: Int,
    characterMagic :: Int
}
-- data ConditionalDescription = ConditionalDescrition{
--     condition::GameCondition,
--     conditionalDescription ::String,
--     conditionalStateChanges::[StateChange]
-- } deriving(Show, Eq)

newtype ConditionalDescription = ConditionalDescription [(GameCondition, String, [StateChange])] deriving (Show, Eq)

data ConditionalAction = ConditionalAction{
    actionCondition :: GameCondition,
    actionDescription :: ConditionalDescription,
    actionStateChanges :: [StateChange]
}deriving (Eq, Show)

data RoomInteraction = RoomInteraction {
    sentences :: [Sentence],
    actions :: [ConditionalAction] 
}deriving (Show,Eq)

data Room = Room {
    roomName :: String,
    roomDescription :: ConditionalDescription,
    roomInteractions :: [RoomInteraction]
} deriving(Show,Eq)


data World = World{
    rooms :: Data.Map.Map String Room,
    endGames :: [String],
    defaulRoom :: Room
}deriving (Show, Eq)

evalCondition :: GameCondition  -> String -> Inventory -> Labels -> Bool
evalCondition CTrue _ _ _ = True 
evalCondition CFalse _ _ _ = False 
evalCondition (YouAlreadyHaveThisItem item) _ (Inventory inventory) _ 
    = item `elem` inventory
evalCondition (LabelExist label) _  _ (Labels labels)
    = label `elem` labels
evalCondition (ThisRoom room) currentRoom _ _ = room == currentRoom

evalCondition (CNot condition) room inventory flags 
    = not (evalCondition condition room inventory flags) 
evalCondition (COr condition1 condition2) room inventory flags 
    = evalCondition condition1 room inventory flags || evalCondition condition2 room inventory flags
evalCondition (CAnd condition1 condition2) room inventory flags 
    = evalCondition condition1 room inventory flags && evalCondition condition2 room inventory flags

-- Imprimir el inventario actual
printInventory :: Inventory -> IO ()
printInventory (Inventory []) = do 
                            putStr "\n" 
                            hFlush stdout
                            

printInventory (Inventory (item:restItems)) = do
                                            putStr "*"
                                            putStrLn  item 
                                            printInventory (Inventory restItems)
                                            

printInvantoryMain :: Inventory -> IO ()
printInvantoryMain inventory = putStrLn "Inventory:\n" >> printInventory inventory 



getPairs :: Applicative f => f a1 -> f a2 -> f (a1, a2)
getPairs a b = (,) <$> a <*> b

matchInteraction :: (RoomInteraction, Sentence) -> Bool
matchInteraction (RoomInteraction {sentences = roomSentences}, sentence)
    | sentence `elem` roomSentences = True
    | otherwise = False

interactionSearch :: [RoomInteraction] -> [Sentence] -> Maybe RoomInteraction
interactionSearch interactions sentences = 
    find matchInteraction (getPairs interactions sentences) >>= (\(x,y)-> Just x) 


searchInDicc :: Ord k => k -> Data.Map.Map k a -> Maybe a
searchInDicc id map = Data.Map.lookup id map

printRoomDescription :: World-> Maybe ([Char], Inventory, Labels)-> IO (Maybe (String, Inventory, Labels))
printRoomDescription World{rooms = gameRooms, endGames = endScenes} Nothing
    =return Nothing


printRoomDescription World{rooms = gameRooms, endGames = endScenes} (Just (roomId, inventory,flags))
    = let room = searchInDicc roomId gameRooms
        in case room of
            Nothing -> putStrLn (roomId ++ "it not a valid room") >> return Nothing
            Just Room {roomDescription = description}
                -> printConditionalDescription  endScenes description [] (Just (roomId, inventory, flags))


updateGameState :: [String]-> String-> Inventory-> Labels-> ConditionalAction-> IO (Maybe (String, Inventory, Labels))
updateGameState endGames roomId inventory labels conditionalAction@(
    ConditionalAction {actionDescription = description, actionStateChanges = stateChanges})
    = printConditionalDescription endGames description [] (Just (roomId, inventory, labels)) >>=
        stateChange (Data.List.find (\x -> case x of 
                                            (RoomChange _) -> True
                                            otherwise -> False) stateChanges)
                    endGames
                    stateChanges



filterInteraction Room {roomInteractions = thisRoomInteraction} Room {roomInteractions = defaultRoomInteractions}
                roomId endGames inventory flags sentences
            = applyConditionalActions roomId endGames inventory flags interaction defaultInteraction
                where (interaction, defaultInteraction) = (interactionSearch thisRoomInteraction sentences,interactionSearch defaultRoomInteractions sentences)


printText :: [String] -> IO ()
printText [] = putStr ""
printText (str:xs) = printText xs >> putStr str

printConditionalDescription :: [String] -> ConditionalDescription -> [String] -> Maybe (String, Inventory, Labels) -> IO (Maybe (String, Inventory, Labels))
printConditionalDescription _ (ConditionalDescription []) lines Nothing
    = printText lines >> putStr "\n" >> return Nothing -- Se acabo el juego


printConditionalDescription _ (ConditionalDescription []) lines (Just (roomId, inventory, flags))
    = printText lines >> putStr "\n" >>hFlush stdout >> return (Just (roomId, inventory, flags) )

printConditionalDescription  _ (ConditionalDescription ((_ ,_ ,_ ):otherDescriptions)) lines Nothing
    = printText lines >> putStr "\n" >> return Nothing -- Se acabo el juego

printConditionalDescription  endGames (ConditionalDescription ((condition , description ,stateChanges):otherDescriptions))
        lines  (Just (roomId, inventory, flags))
    | evalCondition condition roomId inventory flags 
        = stateChange (Data.List.find (\x -> case x of
                                        (RoomChange _) -> True
                                        otherwise ->  False) stateChanges)
            endGames
            stateChanges
            (Just (roomId, inventory, flags)) >>=
                printConditionalDescription endGames (ConditionalDescription otherDescriptions) ((description ++ " ") : lines) 
    | otherwise
        = printConditionalDescription endGames (ConditionalDescription otherDescriptions) lines (Just (roomId, inventory, flags))




-- findAllInteractions :: (Foldable t, Applicative t) =>t RoomInteraction -> t Sentence -> Maybe RoomInteraction
-- findAllInteractions interactions sentences = 
--     find matchInteraction (getPairs interactions sentences) >>= (\(x,y)-> Just x)


applyConditionalActions :: [Char]-> [String]-> Inventory-> Labels-> Maybe RoomInteraction-> Maybe RoomInteraction-> IO (Maybe ([Char], Inventory, Labels))
applyConditionalActions roomId _ inventory flags Nothing Nothing
    = do
        putStr "You are not doing anything"
        hFlush stdout
        return (Just (roomId, inventory, flags))

applyConditionalActions roomId endGames inventory flags (Just RoomInteraction {sentences = _, actions =[]}) defaultRoomInteractions
    = applyConditionalActions roomId endGames inventory flags Nothing defaultRoomInteractions

applyConditionalActions roomId endGames inventory flags 
    (Just RoomInteraction {sentences = thisSentences,
    actions = (conditionalAction@(ConditionalAction {actionCondition = thisCondition}) : remainingConditionalActions)}) defaultRoomInteractions
    | evalCondition thisCondition roomId inventory flags = updateGameState endGames roomId inventory flags conditionalAction
    | otherwise = applyConditionalActions roomId endGames inventory flags ( Just (RoomInteraction {sentences = thisSentences,
                                                                actions = remainingConditionalActions})) defaultRoomInteractions

applyConditionalActions roomId endGames inventory flags
    Nothing (Just RoomInteraction {actions = []})
    = applyConditionalActions roomId endGames inventory flags Nothing Nothing

applyConditionalActions roomId endGames inventory flags 
    Nothing (Just (RoomInteraction{sentences = thisSentences, actions = (conditionalAction@(ConditionalAction {actionCondition = thisCondition}):remainingConditionalActions)}))
    | evalCondition thisCondition roomId inventory flags = updateGameState endGames roomId inventory flags conditionalAction
    | otherwise = applyConditionalActions roomId endGames inventory flags Nothing
                    (Just (RoomInteraction {sentences = thisSentences, actions = remainingConditionalActions}))

newInvRemoveItem :: Inventory -> String -> Inventory
newInvRemoveItem(Inventory items) item = Inventory (filter (/= item)  items)

newInvAddItem :: Inventory -> String -> Inventory
newInvAddItem (Inventory items) newItem 
    | newItem `elem` items = Inventory items
    | otherwise = Inventory (items ++ [newItem])
-- Actualizar el inventario de objetos del jugador
updateInventory :: Inventory -> [StateChange] -> Inventory
updateInventory (Inventory inventory) [] = Inventory inventory

updateInventory currentInventory@(Inventory inventory) ((RemoveItemToInventory item) : otherChanges)
    = updateInventory (newInvRemoveItem currentInventory item ) otherChanges

updateInventory currentInventory@(Inventory inventory) ((AddItemToInventory item) : otherChanges)
    = updateInventory (newInvAddItem currentInventory item ) otherChanges

updateInventory currentInventory@(Inventory inventory) (_ : otherChanges) 
    = updateInventory currentInventory otherChanges

-----------------------------------------------------------------------------------------
newLabelsDel :: Labels -> String -> Labels
newLabelsDel (Labels labels) label = Labels (filter (/= label) labels)

newLabelsAdd :: Labels -> String -> Labels
newLabelsAdd (Labels labels) newLabel
    | newLabel `elem` labels = Labels labels
    | otherwise = Labels (labels ++ [newLabel])

updateLabels :: Labels -> [StateChange] -> Labels
updateLabels currentLabels@(Labels labels) [] = currentLabels
updateLabels currentLabels@(Labels labels) ((RemoveTag label) : otherChanges)
    = updateLabels (newLabelsDel currentLabels label) otherChanges

updateLabels currentLabels@(Labels labels) ((AddTag label) : otherChanges )
    = updateLabels (newLabelsAdd currentLabels label) otherChanges

updateLabels currentLabels@(Labels labels) (_ : otherChanges )
    = updateLabels currentLabels otherChanges

stateChange :: Maybe  StateChange -> [String] -> [StateChange] -> Maybe (String, Inventory, Labels) -> IO (Maybe (String, Inventory, Labels))
stateChange Nothing _ stateChanges Nothing
    = return Nothing

stateChange _ endGames stateChanges Nothing
    = return Nothing

stateChange Nothing _ stateChanges (Just (roomId, inventory, labels))
    = do
        printInventoryFromStates stateChanges inventory
        return (Just (roomId, updateInventory inventory stateChanges,updateLabels labels stateChanges))


stateChange (Just (RoomChange nextRoom)) endGames stateChanges (Just (roomId, inventory, labels))
    = if nextRoom `elem` endGames
        then putStr "IO" >> return Nothing 
    else
        do
        printInventoryFromStates stateChanges inventory
        return (Just (nextRoom, updateInventory inventory stateChanges, updateLabels labels stateChanges))

-- For exhaustic Pattern Matching
stateChange _ _ _ _ = return Nothing

printInventoryFromStates :: [StateChange] -> Inventory -> IO ()
printInventoryFromStates [] inventory =  putStr ""
printInventoryFromStates (PrintInventory : otherchanges) inventory =
    printInvantoryMain inventory >> printInventoryFromStates otherchanges inventory
printInventoryFromStates (_:otherchanges) inventory=
    printInventoryFromStates otherchanges inventory


performInteraction :: World -> String -> Inventory -> Labels -> [Sentence] -> IO (Maybe (String, Inventory, Labels))
performInteraction _ roomId inventory labels []
    = putStrLn "Please enter a command.">>
    hFlush stdout>>
    return (Just (roomId, inventory, labels))

performInteraction worl@(World{rooms=gameRooms, endGames = gameEnd, defaulRoom = thisDefaultRoom}) roomId inventory labels sentences
    = let room = searchInDicc roomId gameRooms
        in case room of
            Nothing -> hFlush stdout >> putStrLn (roomId ++ "is not a valid room") >> return Nothing 
            Just newRoom -> hFlush stdout >> filterInteraction newRoom thisDefaultRoom roomId gameEnd inventory labels sentences


hasInvalidInteractions :: [RoomInteraction] -> Maybe RoomInteraction
hasInvalidInteractions [] = Nothing
hasInvalidInteractions (interaction@(RoomInteraction {sentences = thisSentences}) : remainingInteractions)
    | NullSentence `elem` thisSentences = Just interaction
    | otherwise = hasInvalidInteractions remainingInteractions

printInvalidInteractions :: World -> String -> IO ()
printInvalidInteractions world@(World {rooms = thisRooms}) roomId
    = let room = searchInDicc roomId thisRooms 
        in case room of
        Nothing -> putStrLn (roomId ++ " is not a valid scene") >> return ()
        Just Room {roomInteractions = thisInteractions}
            -> case hasInvalidInteractions thisInteractions of
                    Nothing -> return ()
                    Just interaction@(RoomInteraction {sentences = thisSentences}) -> putStrLn ("Invalid interaction: " ++  (show interaction))


createWorld :: Data.Map.Map String Room -> [String] -> Room -> World
createWorld rooms ends defaulRoom = World{rooms = rooms, endGames =ends, defaulRoom = defaulRoom}
