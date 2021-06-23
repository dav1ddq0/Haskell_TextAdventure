module GameData where

import GameModeling
import GameParser
import GameLexer

import Data.Map

intro :: [Char]
intro = "At the top of a mountain in Dhaka a desolate traveler sees an object of strange value when suddenly\n" ++ 
        "a mysterious portal opens in front of him that leads to the daconic world of Fireblood.\n"++ 
        "A mysterious force pushes him and he appears suddenly in an unexpected room of the same.\n\n\n\n"

verbs :: [Token]
verbs = [
    Verb "get" ["get", "take"],
    Verb "view" ["view"],
    Verb "jump" ["jump"],
    Verb "open" ["open"],
    Verb "walk" ["walk", "move", "go"]
    ]

nouns :: [Token]
nouns = [
    Noun "fire" ["fire"],
    Noun "north" ["north"],
    Noun "south" ["south"],
    Noun "east" ["east"],
    Noun "key" ["key"],
    Noun "door" ["door"],
    Noun "inventory" ["inventory"]
    ]

prepositions :: [Token]
prepositions =[
    Preposition "in" ["in", "inside", "within"],
    Preposition "into" ["into"]
    ]

worldWords :: [Token]
worldWords = nouns ++ verbs ++ prepositions 
startRoom :: [Char]
startRoom = "default"
startInventory :: Inventory
startInventory = Inventory ["ultra ball"]

getMeaningfulSentences :: [String] -> Sentence
getMeaningfulSentences  = meaningfulSentence verbs nouns prepositions 

startLabels :: Labels
startLabels = Labels ["started game"]

initialRoom :: Room
initialRoom = Room {
    roomName = "Dir",
    roomDescription =
        ConditionalDescription [
            (CTrue, "you find yourself in a room a bit desolate", []),
            (CNot (LabelExist  "opened white door"), "The white door is closed", []),
            (LabelExist  "opened white door", "The white door is open",[]),
            (CNot (YouAlreadyHaveThisItem "fire"), "There is a fire power on the floor",[])
            
        ],

        roomInteractions =
            [
                RoomInteraction{
                    sentences = [getMeaningfulSentences ["get", "fire"]],
                    actions =[
                        ConditionalAction{
                            actionCondition = CNot (YouAlreadyHaveThisItem "fire"),
                            actionDescription = ConditionalDescription [(CTrue, "You get the fire", [])],
                            actionStateChanges =[AddItemToInventory "fire"]

                        },
                        ConditionalAction{
                            actionCondition = CTrue,
                            actionDescription = ConditionalDescription [(CTrue, "You already have the key",[])],
                            actionStateChanges =[RoomChange "scene1"]
                        }
                    ]
                },
                RoomInteraction {
                sentences = [getMeaningfulSentences ["view","inventory"]],
                actions =[
                    ConditionalAction{
                        actionCondition = CTrue,
                        actionDescription  = ConditionalDescription [(CTrue, "Imprimir inventario", [])],
                        actionStateChanges = [PrintInventory]
                    }
                ]
            }
            ]
}

communRoom :: Room
communRoom = Room{
    roomName ="I",
    roomDescription = ConditionalDescription [],
    roomInteractions =
        [
            RoomInteraction {
                sentences = [getMeaningfulSentences ["jump"]],
                actions =[
                    ConditionalAction{
                        actionCondition = CTrue,
                        actionDescription  = ConditionalDescription [(CTrue, "You jump up and down in the place", [])],
                        actionStateChanges = []
                    }
                ]
            }
            
        ]
}

scene1 :: Room
scene1 =
    Room
    {
        roomName = "UI",
        roomDescription = ConditionalDescription [(CTrue, "", [])],
        roomInteractions = []
    }

roomsMap :: (Map [Char] Room, [[Char]])
roomsMap = (Data.Map.fromList [("default",initialRoom),("scen1", scene1)], ["scene1"])