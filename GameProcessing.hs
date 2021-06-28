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

evalCondition LowHealth World{player=Player{playerLife = thisLife}} _ 
    = thisLife < 20

evalCondition YouHaveDied  World{player=Player{playerLife = thisLife}} _ 
    = thisLife == 0

evalCondition LowEnergy World{player=Player{playerMagic = thisMagic}} _ 
    = thisMagic == 0

evalCondition FullHealth  World{player=Player{playerLife  = thisLife}} _ 
    =  thisLife == 100

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
printLocationDescription :: Maybe Location-> IO ()
printLocationDescription Nothing = putStr ""
printLocationDescription (Just Location{locationDescription=thisDescription}) =
    putStr thisDescription


-- Execute  each action given a list of actions
-- ---------------------------------------------------------------------------------
exeStageAction :: [InteractionAction]-> World -> [Char] -> IO (Maybe (World, [Char]))
exeStageAction [] world locationId =
    return (Just(world,locationId))
    
exeStageAction (InteractionAction{
    actionDescription = thisDescription,
    actionGameActions = thisGameActions}:otherStageActions) 
    world locationId
    = do
        printDescription thisDescription
        outEx <-exeGameAction thisGameActions world locationId
        if outEx == Nothing 
            then 
                return Nothing
        else
            do
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

-- Remove item from the player bag
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Remove only the first occurrence of the item if it exists

removeItemFromBagAction :: Eq a => [a] -> a -> [a]
removeItemFromBagAction [] itemToRemove = []
removeItemFromBagAction (item :otherItems) itemToRemove
    | item == itemToRemove = otherItems
    | otherwise = item:removeItemFromBagAction otherItems itemToRemove
-- Print current player bag

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

-- Tags Actions
-- -------------------------------------------------------------------
-- Add new tag to the list of tags of the world

addNewTag :: Eq a => [a] -> a -> [a]
addNewTag tags newTag
    | newTag `elem` tags = tags
    | otherwise = tags ++ [newTag]

-- Remove a tag from the list of tags of the world
removeTag :: Eq a => [a] -> a -> [a]
removeTag tags tag = filter (/= tag)  tags

-- -------------------------------------------------------------------
-- Life Actions
-- -------------------------------------------------------------------
updateLife :: (Ord p, Num p) => p -> p -> p
updateLife damage life | (life - damage) < 0 = 0
                        | otherwise = life - damage

recoveryLife :: (Ord p, Num p) => p -> p -> p
recoveryLife val life | (life + val) > 100 = 100
                    | otherwise  = life + val
-- --------------------------------------------------------------------
-- Magic Actions
updateMagic :: (Ord p, Num p) => p -> p -> p
updateMagic less magic | (magic - less) < 0 = 0
                        |otherwise = magic - less

recoveryMagic :: (Ord p, Num p) => p -> p -> p
recoveryMagic val magic | (magic + val) > 100 = 100
                        | otherwise = magic + val
-- --------------------------------------------------------------------
-- Print Status of the Player:

printPlayerStatus :: (Show a1, Show a2) => [Char] -> a1 -> a2 -> IO ()
printPlayerStatus name life magic =
    putStr ("Player Status \nNAME: " ++ name ++ "\nLIFE: " ++ show life ++ "\nMAGIC: "++ show magic ++ "\n") 
-- ---------------------------------------------------------------------
-- Print your name

printPlayerName :: String -> IO ()
printPlayerName  =
    putStrLn 

-- --------------------------------------------------------------------
printHelp :: IO ()
printHelp = 
    putStr ("*HELP*:\n" ++ 
    "-type <view status> to see the status of the player\n"++
    "-type <exit> to force exit game\n"++
    "-type <who Iam> to see playe name\n"++
    "-type <use potion> to recover the player's life in case you have in the bag\n"++
    "-type <give me the time> to know the current time\n"++
    "-type <use energy drink> to recover the player's magic in case you have in the bag\n"++
    "-type <help> to see this help")
-- -----------------------------------------------------------------------------------------
printExit :: IO ()
printExit =
    putStrLn "You have forced out of the game\n"

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Execute all the game actions of the interaction action what matched
-- -----------------------------------------------------------------------------
exeGameAction :: [GameAction] -> World -> String -> IO (Maybe (World, String))
exeGameAction [] world locationId  = 
    return (Just(world, locationId))

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
        locationId
    = do 
        let newBag =  addItemToBagAction thisBag item
        exeGameAction otherGameAction (World{
            locations = thisStages,
            player = (Player{playerName = thisPlayerName,playerLife = thisPlayerLife,playerMagic = thisPlayerMagic,bag = newBag}),
            tags = thisTags,
            endGames = thisEnds,
            communActions = thisDefault}) locationId

exeGameAction ((RemoveItemFromBag item) : otherGameAction) 
    World{locations = thisLocations,
        player = Player{
            playerName = thisPlayerName,
            playerLife = thisPlayerLife,
            playerMagic = thisPlayerMagic,
            bag = thisInventory},
        tags = thisTags,
        endGames =thisEnds,
        communActions = thisDefault}
        locationId
    = do 
        let newBag =  removeItemFromBagAction thisInventory item
        exeGameAction otherGameAction (World{
            locations = thisLocations,
            player = (Player{playerName = thisPlayerName,playerLife = thisPlayerLife,playerMagic = thisPlayerMagic,bag= newBag}),
            tags = thisTags,
            endGames = thisEnds,
            communActions = thisDefault}) locationId

exeGameAction (PrintBag  : otherGameActions) 
    world@World{locations = thisLocations,
        player = Player{
            playerName = thisPlayerName,
            playerLife = thisPlayerLife,
            playerMagic = thisPlayerMagic,
            bag = thisBag},
        tags = thisTags,
        endGames =thisEnds,
        communActions = thisDefault}
        locationId
    = do 
        printBagMain thisBag thisPlayerName
        exeGameAction otherGameActions world locationId
        

exeGameAction (GTime  : otherGameActions) world locationId
    = do 
        getGameTime 
        exeGameAction otherGameActions world locationId

exeGameAction (AddTag tag  : otherGameActions) 
    World{locations = thisLocations,
        player =thisPlayer,
        tags = thisTags,
        endGames =thisEnds,
        communActions = thisDefault}
        locationId
    = do 
        let newTags = addNewTag thisTags tag
        exeGameAction otherGameActions
            World{locations = thisLocations,
            player =thisPlayer,
            tags = newTags,
            endGames =thisEnds,
            communActions = thisDefault}  locationId

exeGameAction (RemoveTag  tag  : otherGameActions) 
    World{locations = thisLocations,
        player =thisPlayer,
        tags = thisTags,
        endGames =thisEnds,
        communActions = thisDefault}
        locationId
    = do 
        let newTags = removeTag thisTags tag
        exeGameAction otherGameActions
            World{locations = thisLocations,
            player =thisPlayer,
            tags = newTags,
            endGames =thisEnds,
            communActions = thisDefault}  locationId

exeGameAction (NextLocation newLocationId: otherGameActions) 
    world@World{locations = thisLocations,
        player =thisPlayer,
        tags = thisTags,
        endGames =thisEnds,
        communActions = thisDefault}
        locationId
    = do 
        if newLocationId `elem` thisEnds 
            then return Nothing
        else do
            printLocationDescription (getLocationFromId world newLocationId)
            return (Just (world,newLocationId))

exeGameAction (RDamage damage: otherGameActions) 
    world@World{locations = thisLocations,
        player =Player{
            playerName = thisPlayerName,
            playerLife = thisPlayerLife,
            playerMagic = thisPlayerMagic,
            bag = thisBag
        },
        tags = thisTags,
        endGames =thisEnds,
        communActions = thisDefault}
        locationId
    = do 
        let newLife = updateLife damage thisPlayerLife
        if newLife == 0 
            then do
                printRunOutOfLife
                return Nothing
        else
            exeGameAction otherGameActions (World{
            locations = thisLocations,
            player = (Player{playerName = thisPlayerName,playerLife = newLife,playerMagic = thisPlayerMagic,bag= thisBag}),
            tags = thisTags,
            endGames = thisEnds,
            communActions = thisDefault}) locationId
        

exeGameAction (RecLife val: otherGameActions) 
    world@World{locations = thisLocations,
        player =Player{
            playerName = thisPlayerName,
            playerLife = thisPlayerLife,
            playerMagic = thisPlayerMagic,
            bag = thisBag
        },
        tags = thisTags,
        endGames =thisEnds,
        communActions = thisDefault}
        locationId
    = do 
        let newLife = recoveryLife val thisPlayerLife
        exeGameAction otherGameActions (World{
        locations = thisLocations,
        player = (Player{playerName = thisPlayerName,playerLife = newLife,playerMagic = thisPlayerMagic,bag= thisBag}),
        tags = thisTags,
        endGames = thisEnds,
        communActions = thisDefault}) locationId

exeGameAction (WasteEnergy energy: otherGameActions) 
    world@World{locations = thisLocations,
        player =Player{
            playerName = thisPlayerName,
            playerLife = thisPlayerLife,
            playerMagic = thisPlayerMagic,
            bag = thisBag
        },
        tags = thisTags,
        endGames =thisEnds,
        communActions = thisDefault}
        locationId
    = do 
        let newMagic = updateMagic energy thisPlayerMagic
        exeGameAction otherGameActions (World{
        locations = thisLocations,
        player = (Player{playerName = thisPlayerName,playerLife = thisPlayerLife, playerMagic = newMagic, bag= thisBag}),
        tags = thisTags,
        endGames = thisEnds,
        communActions = thisDefault}) locationId

exeGameAction (RecoveryEnergy energy: otherGameActions) 
    world@World{locations = thisLocations,
        player =Player{
            playerName = thisPlayerName,
            playerLife = thisPlayerLife,
            playerMagic = thisPlayerMagic,
            bag = thisBag
        },
        tags = thisTags,
        endGames =thisEnds,
        communActions = thisDefault}
        locationId
    = do 
        let newMagic = recoveryMagic energy thisPlayerMagic
        exeGameAction otherGameActions (World{
        locations = thisLocations,
        player = (Player{playerName = thisPlayerName,playerLife = thisPlayerLife, playerMagic = newMagic, bag= thisBag}),
        tags = thisTags,
        endGames = thisEnds,
        communActions = thisDefault}) locationId

exeGameAction (Status : otherGameActions) 
    world@World{locations = thisLocations,
        player =Player{
            playerName = thisPlayerName,
            playerLife = thisPlayerLife,
            playerMagic = thisPlayerMagic,
            bag = thisBag
        },
        tags = thisTags,
        endGames =thisEnds,
        communActions = thisDefault}
        locationId
    = do 
        printPlayerStatus thisPlayerName thisPlayerLife thisPlayerMagic
        exeGameAction otherGameActions world locationId 

exeGameAction (PName  : otherGameActions) 
    world@World{locations = thisLocations,
        player =Player{
            playerName = thisPlayerName,
            playerLife = thisPlayerLife,
            playerMagic = thisPlayerMagic,
            bag = thisBag
        },
        tags = thisTags,
        endGames =thisEnds,
        communActions = thisDefault}
        locationId
    = do 
        printPlayerName thisPlayerName
        exeGameAction otherGameActions world locationId 


exeGameAction (Help  : otherGameActions) 
    world locationId
    = do 
        printHelp
        exeGameAction otherGameActions world locationId 

exeGameAction (Exit: otherGameActions) 
    world locationId
    = do 
        printExit
        return Nothing

-- exhaustic pattern mathing
exeGameAction _ _ _ = return Nothing 

-- ------------------------------------------------------------------------------



    
printDescription :: String -> IO ()
printDescription  = putStrLn 


getWorldFromMaybe :: Monad m => Maybe (World, b) -> m World
getWorldFromMaybe input = case input of
    Nothing -> return World{
        locations =Data.Map.empty,
        tags = [],
        endGames = [], 
        communActions = Location "" "" [],
        player = Player{
            playerName = "",
            playerLife = 0,
            playerMagic =0,
            bag= []
        }

    }

    
    Just (world, locationId) -> return world

getRoomIdFromMaybeT :: Monad m => Maybe (a, [Char]) -> m [Char]
getRoomIdFromMaybeT input = case input of
    Nothing -> return ""
    Just (world, locationId) -> return locationId




getNotMaybeRoomId :: Monad m => Maybe Location -> m Location
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

-- Dado un id de location valido devuelve el locaction que corresponde al mismo
-- ----------------------------------------------------------------------------------------------
getLocationFromId :: World -> String -> Maybe Location
getLocationFromId World{locations = gameLocations} locationId = searchInDicc locationId gameLocations
-- -----------------------------------------------------------------------------------------------
printNotInteractionFoundError :: IO ()
printNotInteractionFoundError =
    putStr "Are you sure you wanted to tell me that?\n"

performInteraction :: World -> [Char] -> [Sentence] -> IO (Maybe (World, [Char]))
performInteraction world locationId  []
    = putStrLn "Please enter a command.">>
    hFlush stdout>>
    return (Just (world, locationId))



performInteraction world locationId  sentences
    = let location = getLocationFromId world locationId
        in case location of
            Nothing -> hFlush stdout 
                >> putStrLn ("The location" ++ locationId ++ "is not a valid location") 
                >> return Nothing 
            Just newRoom -> hFlush stdout >>
                do 
                interactions  <- getMatchedInteractions world newRoom sentences

                if interactions == Nothing  
                    then do
                        printNotInteractionFoundError
                        return (Just (world, locationId))
                else
                    do
                        
                        pureInteraction <- getNotMaybeRoomIntegration interactions
                        pureNotMaybeRoom <- getNotMaybeRoomId location
                        let actions = getValidConditionalActions world pureNotMaybeRoom pureInteraction
                        exeStageAction actions world locationId 

