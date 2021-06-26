module GameModeling where
import GameParser
import Data.List
import qualified Data.List
import qualified Data.Map
import System.IO
import GameUtils

type Tags = [String]
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
    tags :: Tags,
    endGames :: [String],
    communActions :: Location
}deriving (Show, Eq)




getRoomId :: Monad m => Maybe ([Char], b, c) -> m [Char]
getRoomId update = case update of
        Nothing ->  return ""
        Just (roomId, inventory, tags) -> return roomId


getRoomFromId :: World -> String -> Maybe Location
getRoomFromId World{locations = gameLocations} roomId = searchInDicc roomId gameLocations


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


