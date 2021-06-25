module GameData where

import GameModeling
import GameParser
import GameLexer
import Data.Time.Clock
import Data.Map
import System.IO
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
    Noun "bag" ["bag"],
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
startLocation :: [Char]
startLocation = "azukiarai"

getMeaningfulSentences :: [String] -> Sentence
getMeaningfulSentences  = meaningfulSentence verbs nouns prepositions 

azukiarai :: Location
azukiarai = Location {
    locationId = "Azukiarai",
    locationDescription = "You find yourself in a place a bit desolate like a cave\n",
    locationInteractions =
        [
            LocationInteraction{
                interactionSentences = [getMeaningfulSentences ["look", "around"]],
                interactionActions =[
                    InteractionAction{
                        actionCondition = GameNot (YouAlreadyHaveThisItem "rhydon drill"),
                        actionDescription = "There is a strange rhydon drill object near the cave mosses.\n",
                        actionGameActions  =[]
                    },
                    InteractionAction{
                        actionCondition = GameNot (YouAlreadyHaveThisItem "magick stick"),
                        actionDescription = "There is a magic stick near the lava river.\n",
                        actionGameActions = []
                    },
                    InteractionAction{
                        actionCondition =  GameNot (TagExist  "rock moved"), 
                        actionDescription = "There is a big rock blocking the exit\n",
                        actionGameActions  =  []
                    },
                    InteractionAction{
                        actionCondition = TagExist  "rock moved",
                        actionDescription = "The big rock was removed the exit is clear\n",
                        actionGameActions = []
                    }
                    
                ]
            },
            LocationInteraction{
                interactionSentences = [getMeaningfulSentences ["get", "rhydon drill"]],
                interactionActions =[
                    InteractionAction{
                        actionCondition = GameNot (YouAlreadyHaveThisItem "rhydon drill"),
                        actionDescription = "You get rhydon drill item\n",
                        actionGameActions = [AddItemToBag "rhydon drill"]
                    },
                    InteractionAction{
                        actionCondition = YouAlreadyHaveThisItem "rhydon drill",
                        actionDescription = "This object has already been taken\n",
                        actionGameActions = []
                    }
                ]
            },
            LocationInteraction{
                interactionSentences = [getMeaningfulSentences ["get", "magick stick"]],
                interactionActions =[
                    InteractionAction{
                        actionCondition = GameNot (YouAlreadyHaveThisItem "magick stick"),
                        actionDescription = "You get magick stick item\n",
                        actionGameActions = [AddItemToBag "magick stick"]
                    },
                    InteractionAction{
                        actionCondition = YouAlreadyHaveThisItem "magick stick",
                        actionDescription = "This object has already been taken\n",
                        actionGameActions  = []
                    }
                ]
            },
            LocationInteraction{
                interactionSentences = [getMeaningfulSentences ["go to", "Xerneas spectrum"]],
                interactionActions =[
                    InteractionAction{
                        actionCondition = GameNot (TagExist  "You already spoke with Xerneas"),
                        actionDescription = 
                        "Welcome young traveler to fireblood world of desolation.\n" ++
                        "Take this mantra fusion power to forge new items that will serve you on your journey.\n",
                        actionGameActions  =[AddTag  "You got mantra fusion", AddTag "You already spoke with Xerneas"]
                    },
                    InteractionAction{
                        actionCondition = TagExist  "You already spoke with Xerneas",
                        actionDescription =  "This object has already been taken\n",
                        actionGameActions  =[]
                    }
                ]
            },
            LocationInteraction{
                interactionSentences = [getMeaningfulSentences ["use", "mantra fusion", "with", "[magick stick, rhydon drill]"],
                            getMeaningfulSentences ["use", "mantra fusion", "with", "[rhydon drill, magick stick]"]],
                
                interactionActions =[
                    InteractionAction{
                        actionCondition = GameNot (TagExist  "You got mantra fusion"),
                        actionDescription = "I do not know  mantra fusion power.\n", 
                        actionGameActions  =[]
                    },
                    InteractionAction{
                        actionCondition = GameNot (YouAlreadyHaveThisItem "magick stick"),
                        actionDescription = "I don't have the item magick stick in my inventory.\n", 
                        actionGameActions  =[]
                    },
                    InteractionAction{
                        actionCondition = GameNot (YouAlreadyHaveThisItem "rhydon drill"),
                        actionDescription =  "I don't have the item rhydon drill in my inventory.\n",
                        actionGameActions  =[]
                    },
                    InteractionAction{
                        actionCondition = GameAnd (YouAlreadyHaveThisItem "rhydon drill")
                        (GameAnd (YouAlreadyHaveThisItem "magick stick") (TagExist  "You got mantra fusion")),
                        actionDescription =  "The mantra fusion magic began\n"++
                        "....\n"++ "A strong light has emerged.\n" ++ "You have obtained a new item:\n rockbreaker",
                        actionGameActions  =[AddItemToBag "rockbreaker", RemoveItemFromBag "rhydon drill", RemoveItemFromBag "magick stick"]
                    }
                ]
            }
            

            
            ]
}



commonActions :: Location
commonActions= Location{
    locationId ="default",
    locationDescription = "",
    locationInteractions =
        [
            LocationInteraction {
                interactionSentences = [getMeaningfulSentences ["jump"]],
                interactionActions =[
                    InteractionAction {
                        actionCondition = GameTrue,
                        actionDescription  ="You jump up and down in the place",
                        actionGameActions  = []
                    }
                ]
            },

            LocationInteraction {
                interactionSentences = [getMeaningfulSentences ["view","bag"]],
                interactionActions =[
                    InteractionAction {
                        actionCondition = GameTrue,
                        actionDescription  = "",
                        actionGameActions  = [PrintBag]
                    }
                ]
            },

            LocationInteraction {
                interactionSentences = [getMeaningfulSentences ["give me","the", "time"]],
                interactionActions =[
                    InteractionAction{
                        actionCondition = GameTrue,
                        actionDescription  = ""  ,
                        actionGameActions  = [GTime]
                    }
                ]
            }
            
        ]
}

scene1 :: Location
scene1 =
    Location
    {
        locationId = "UI",
        locationDescription = "",
        locationInteractions = []
    }



createWorld :: Data.Map.Map String Location-> [String] -> Location -> Player  -> World
createWorld locations ends defaulRoom player= 
    World{locations = locations,
    player = player,
    tags = ["game started"],
    endGames = ends, 
    communActions = defaulRoom
    }



buildPlayer :: IO Player
buildPlayer = do 
    putStr "Write the name of the player: "
    hFlush stdout
    createPlayer <$> getLine
    
createPlayer :: String -> Player
createPlayer name  =
    Player{
        playerName = name,
        playerLife = 100,
        playerMagic =100,
        bag= ["pocion"]
    }



locationsMap :: (Map [Char] Location , [[Char]])
locationsMap = (Data.Map.fromList [("azukiarai",azukiarai),("scen1", scene1)], ["scene1"])