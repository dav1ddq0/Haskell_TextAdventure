module GameData where

import GameModeling
import GameParser
import GameLexer
import Data.Time.Clock
import Data.Map

intro :: [Char]
intro = "\n At the top of a mountain in Dhaka a desolate traveler sees an object of strange value when suddenly\n" ++ 
        "a mysterious portal opens in front of him that leads to the daconic world of Fireblood.\n"++ 
        "A mysterious force pushes him and he appears suddenly in an unexpected place of the same.\n\n"

verbs :: [Token]
verbs = [
    Verb "get" ["get", "take"],
    Verb "view" ["view"],
    Verb "jump" ["jump"],
    Verb "open" ["open"],
    Verb "walk" ["walk", "move", "go"],
    Verb "look" ["look"],
    Verb "give me" ["give me"],
    Verb "go to" ["go to"],
    Verb "use" ["use"]
    ]

nouns :: [Token]
nouns = [
    Noun "fire" ["fire"],
    Noun "north" ["north"],
    Noun "south" ["south"],
    Noun "east" ["east"],
    Noun "key" ["key"],
    Noun "door" ["door"],
    Noun "inventory" ["inventory"],
    Noun "rhydon drill" ["rhydon drill"],
    Noun "magick stick" ["magic stick"],
    Noun "around" ["around"],
    Noun "time" ["time"],
    Noun "mantra fusion" ["mantra fusion"],
    Noun "[rhydon drill, magick stick]" ["[rhydon drill, magick stick]","[magick stick, rhydon drill]"]
    ]

prepositions :: [Token]
prepositions =[
    Preposition "in" ["in", "inside", "within"],
    Preposition "into" ["into"],
    Preposition "the" ["the"],
    Preposition "with" ["with"]
    ]

worldWords :: [Token]
worldWords = nouns ++ verbs ++ prepositions 
startRoom :: [Char]
startRoom = "default"
startInventory :: Inventory
startInventory = Inventory ["pocion"]

getMeaningfulSentences :: [String] -> Sentence
getMeaningfulSentences  = meaningfulSentence verbs nouns prepositions 

startLabels :: Labels
startLabels = Labels ["started game"]

azukiaraiRoom :: Room
azukiaraiRoom = Room {
    roomName = "Azukiarai",
    roomDescription =
        ConditionalDescription [
            (GameTrue, "You find yourself in a place a bit desolate like a cave\n", [])
        ],

        roomInteractions =
            [
                RoomInteraction{
                    sentences = [getMeaningfulSentences ["look", "around"]],
                    actions =[
                        ConditionalAction{
                            actionCondition = GameNot (YouAlreadyHaveThisItem "rhydon drill"),

                            actionDescription = ConditionalDescription [(GameTrue, "There is a strange rhydon drill object near the cave mosses.\n",[])],
                            actionStateChanges =[]
                        },
                        ConditionalAction{
                            actionCondition = GameNot (YouAlreadyHaveThisItem "magick stick"),

                            actionDescription = ConditionalDescription [(GameTrue, "There is a magic stick near the lava river.\n",[])],
                            actionStateChanges =[]
                        },
                        ConditionalAction{
                            actionCondition =  GameNot (LabelExist  "rock moved"), 
                            actionDescription = ConditionalDescription [(GameTrue, "There is a big rock blocking the exit\n", [])],
                            actionStateChanges =[]

                        },
                        ConditionalAction{
                            actionCondition = LabelExist  "rock moved",
                            actionDescription = ConditionalDescription [(GameTrue, "The big rock was removed the exit is clear\n",[])],
                            actionStateChanges =[]
                        }
                        
                    ]
                },
                RoomInteraction{
                    sentences = [getMeaningfulSentences ["get", "rhydon drill"]],
                    actions =[
                        ConditionalAction{
                            actionCondition = GameNot (YouAlreadyHaveThisItem "rhydon drill"),
                            actionDescription = ConditionalDescription [(GameTrue, "You get rhydon drill item\n", [])],
                            actionStateChanges =[AddItemToInventory "rhydon drill"]

                        },
                        ConditionalAction{
                            actionCondition = YouAlreadyHaveThisItem "rhydon drill",
                            actionDescription = ConditionalDescription [(GameTrue, "This object has already been taken\n",[])],
                            actionStateChanges =[]
                        }
                    ]
                },

                RoomInteraction{
                    sentences = [getMeaningfulSentences ["get", "magick stick"]],
                    actions =[
                        ConditionalAction{
                            actionCondition = GameNot (YouAlreadyHaveThisItem "magick stick"),
                            actionDescription = ConditionalDescription [(GameTrue, "You get magick stick item\n", [])],
                            actionStateChanges =[AddItemToInventory "magick stick"]

                        },
                        ConditionalAction{
                            actionCondition = YouAlreadyHaveThisItem "magick stick",
                            actionDescription = ConditionalDescription [(GameTrue, "This object has already been taken\n",[])],
                            actionStateChanges =[]
                        }
                    ]
                },

                RoomInteraction{
                    sentences = [getMeaningfulSentences ["go to", "Xerneas spectrum"]],
                    actions =[
                        ConditionalAction{
                            actionCondition = GameNot (LabelExist  "You already spoke with Xerneas"),
                            actionDescription = ConditionalDescription [(GameTrue,
                            "Welcome young traveler to fireblood world of desolation.\n" ++
                            "Take this mantra fusion power to forge new items that will serve you on your journey.\n", [])],
                            actionStateChanges =[AddTag  "You got mantra fusion", AddTag "You already spoke with Xerneas"]

                        },
                        ConditionalAction{
                            actionCondition = LabelExist  "You already spoke with Xerneas",
                            actionDescription = ConditionalDescription [(GameTrue, "This object has already been taken\n",[])],
                            actionStateChanges =[]
                        }
                    ]
                },

                RoomInteraction{
                    sentences = [getMeaningfulSentences ["use", "mantra fusion", "with", "[magick stick, rhydon drill]"],
                                getMeaningfulSentences ["use", "mantra fusion", "with", "[rhydon drill, magick stick]"]],
                    actions =[
                        ConditionalAction{
                            actionCondition = GameNot (LabelExist  "You got mantra fusion"),
                            actionDescription = ConditionalDescription [(GameTrue,"I do not know  mantra fusion power.\n",[])], 
                            actionStateChanges =[]

                        },
                        ConditionalAction{
                            actionCondition = GameNot (YouAlreadyHaveThisItem "magick stick"),
                            actionDescription = ConditionalDescription [(GameTrue,"I don't have the item magick stick in my inventory.\n",[])], 
                            actionStateChanges =[]

                        },
                        ConditionalAction{
                            actionCondition = GameNot (YouAlreadyHaveThisItem "rhydon drill"),
                            actionDescription = ConditionalDescription [(GameTrue, "I don't have the item rhydon drill in my inventory.\n",[])],
                            actionStateChanges =[]
                        },
                        ConditionalAction{
                            actionCondition = GameAnd (YouAlreadyHaveThisItem "rhydon drill")
                            (GameAnd (YouAlreadyHaveThisItem "magick stick") (LabelExist  "You got mantra fusion")),
                            actionDescription = ConditionalDescription [(GameTrue, "The mantra fusion magic began\n"++
                            "....\n"++ "A strong light has emerged.\n" ++ "You have obtained a new item:\n rockbreaker",[])],
                            actionStateChanges =[AddItemToInventory "rockbreaker"]
                        }
                    ]
                }



                


                
                
            
            ]
}



commonActions :: Room
commonActions= Room{
    roomName ="default",
    roomDescription = ConditionalDescription [],
    roomInteractions =
        [
            RoomInteraction {
                sentences = [getMeaningfulSentences ["jump"]],
                actions =[
                    ConditionalAction{
                        actionCondition = GameTrue,
                        actionDescription  = ConditionalDescription [(GameTrue, "You jump up and down in the place", [])],
                        actionStateChanges = []
                    }
                ]
            },

            RoomInteraction {
                sentences = [getMeaningfulSentences ["view","inventory"]],
                actions =[
                    ConditionalAction{
                        actionCondition = GameTrue,
                        actionDescription  = ConditionalDescription [(GameTrue, "", [])],
                        actionStateChanges = [PrintInventory]
                    }
                ]
            },

            RoomInteraction {
                sentences = [getMeaningfulSentences ["give me","the", "time"]],
                actions =[
                    ConditionalAction{
                        actionCondition = GameTrue,
                        actionDescription  = ConditionalDescription [(GameTrue, ""  , [])],
                        actionStateChanges = [GTime ]
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
        roomDescription = ConditionalDescription [(GameTrue, "", [])],
        roomInteractions = []
    }

roomsMap :: (Map [Char] Room, [[Char]])
roomsMap = (Data.Map.fromList [("default",azukiaraiRoom),("scen1", scene1)], ["scene1"])