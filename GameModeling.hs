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
                    RDamage Int|
                    RecLife Int |
                    Status |
                    WasteEnergy Int|
                    RecoveryEnergy Int|
                    PName |
                    Help |
                    Exit |
                    NextLocation String deriving (Eq, Show)

data GameCondition = YouAlreadyHaveThisItem String |
                    ThisLocation String|
                    TagExist String|
                    LowHealth |
                    LowEnergy |
                    FullHealth|
                    YouHaveDied|
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



