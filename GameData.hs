module GameData where

import GameModeling
import GameParser
import GameLexer
import Data.Time.Clock
import Data.Map
import System.IO

intro :: Player -> [Char]
intro Player{playerName =thisName}= "\nAt the top of a mountain in Dhaka a desolate traveler called "++thisName ++ " sees an object of strange value when suddenly\n" ++ 
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
    Verb "use" ["use"],
    Verb "break" ["break"],
    Verb "leave" ["leave"]
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
    Noun "magic stick" ["magic stick"],
    Noun "around" ["around"],
    Noun "time" ["time"],
    Noun "big rock" ["big rock"],
    Noun "mantra fusion" ["mantra fusion"],
    Noun "cave" ["cave"],
    Noun "status" ["status"],
    Noun "potion" ["potion"],
    Noun "Xerneas spectrum" ["Xerneas spectrum"],
    Noun "[rhydon drill, magic stick]" ["[rhydon drill, magic stick]","[magic stick, rhydon drill]"]
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
    locationDescription = "*AZUKIARAI CAVE*\nYou find yourself in a place a bit desolate like a cave\n",
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
                        actionCondition = GameNot (YouAlreadyHaveThisItem "magic stick"),
                        actionDescription = "There is a magic stick near the lava river.\n",
                        actionGameActions = []
                    },
                    InteractionAction{
                        actionCondition =  GameNot (TagExist  "rock moved"), 
                        actionDescription = "There is a big rock blocking the exit\n",
                        actionGameActions  =  []
                    },
                    InteractionAction{
                        actionCondition = TagExist  "rock removed",
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
                interactionSentences = [getMeaningfulSentences ["get", "magic stick"]],
                interactionActions =[
                    InteractionAction{
                        actionCondition = GameNot (YouAlreadyHaveThisItem "magic stick"),
                        actionDescription = "You get magic stick item\n",
                        actionGameActions = [AddItemToBag "magic stick"]
                    },
                    InteractionAction{
                        actionCondition = YouAlreadyHaveThisItem "magic stick",
                        actionDescription = "This item has already been taken\n",
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
                interactionSentences = [getMeaningfulSentences ["use", "mantra fusion"]],
                
                interactionActions =[
                    InteractionAction{
                        actionCondition = GameNot (TagExist  "You got mantra fusion"),
                        actionDescription = "I do not know  mantra fusion power.\n", 
                        actionGameActions  =[]
                    },
                    InteractionAction{
                        actionCondition = GameNot (YouAlreadyHaveThisItem "magic stick"),
                        actionDescription = "I don't have the item magic stick in my inventory.\n", 
                        actionGameActions  =[]
                    },
                    InteractionAction{
                        actionCondition = GameNot (YouAlreadyHaveThisItem "rhydon drill"),
                        actionDescription =  "I don't have the item rhydon drill in my inventory.\n",
                        actionGameActions  =[]
                    },
                    InteractionAction{
                        actionCondition = GameAnd (YouAlreadyHaveThisItem "rhydon drill")
                        (GameAnd (YouAlreadyHaveThisItem "magic stick") (TagExist  "You got mantra fusion")),
                        actionDescription =  "The mantra fusion magic began\n"++
                        "....\n"++ "A strong light has emerged.\n" ++ "You have obtained a new item:\n*rockbreaker*",
                        actionGameActions  =[AddItemToBag "rockbreaker", RemoveItemFromBag "rhydon drill", RemoveItemFromBag "magic stick"]
                    }
                ]
            },
            LocationInteraction{
                interactionSentences = [getMeaningfulSentences ["break", "big rock"]],
                
                interactionActions =[
                    InteractionAction{
                        actionCondition = GameAnd (GameNot (TagExist  "rock removed")) (YouAlreadyHaveThisItem "rockbreaker"),
                        actionDescription = "Use rockbreaker to break the big rock that blocks the exit of the cave.\n", 
                        actionGameActions  =[AddTag "rock removed"]
                    },

                    InteractionAction{
                        actionCondition = TagExist  "rock removed",
                        actionDescription = "The big rock has already been removed.\n", 
                        actionGameActions  =[]
                    },

                    InteractionAction{
                        actionCondition = GameNot(YouAlreadyHaveThisItem "rockbreaker"),
                        actionDescription = "You have nothing to remove the rock.\n", 
                        actionGameActions  = []
                    }

                ]

            
            },
            LocationInteraction{
                interactionSentences = [getMeaningfulSentences ["leave", "the", "cave"]],
                
                interactionActions =[
                    InteractionAction{
                        actionCondition = GameNot (TagExist  "rock removed"),
                        actionDescription = "You can't leave the cave.\nA big rock blocks the exit\n", 
                        actionGameActions  =[]
                    },

                    InteractionAction{
                        actionCondition = TagExist  "rock removed",
                        actionDescription = "The traveler manages to get out of the burning cave\n", 
                        actionGameActions  = [NextLocation "casentinesi"]
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
                        actionGameActions  = [RDamage 100]
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
            },

            LocationInteraction {
                interactionSentences = [getMeaningfulSentences ["use","potion"]],
                interactionActions =[
                    InteractionAction{
                        actionCondition = YouAlreadyHaveThisItem "potion",
                        actionDescription  = "The traveler used a potion"  ,
                        actionGameActions  = [RemoveItemFromBag "potion", RecLife 60]
                    },
                    InteractionAction{
                        actionCondition = GameNot(YouAlreadyHaveThisItem "potion"),
                        actionDescription  = "You have no potions left in your bag"  ,
                        actionGameActions  = []
                    }
                ]
                    
                
            },
            LocationInteraction {
                interactionSentences = [getMeaningfulSentences ["view","status"]],
                interactionActions =[
                    InteractionAction{
                        actionCondition = GameTrue ,
                        actionDescription  = ""  ,
                        actionGameActions  = [Status]
                    }
                    
                ]
                    
                
            }

            
        ]
}

casentinesi:: Location
casentinesi =
    Location
    {
        locationId = "casentinesi",
        locationDescription = "*CASENTINESI BLOOD FOREST*\nYou have entered the bloody forest of Casentinesi " ++ 
        "where the trees feed on the blood of the dead ...",
        locationInteractions = [
            LocationInteraction{
                interactionSentences = [getMeaningfulSentences ["go to", "Altar of Elders Darkfire"]],
                
                interactionActions =[
                    InteractionAction{
                        actionCondition = GameNot (TagExist  "Already at the altar"),
                        actionDescription = "The traveler walked to the altar of elders darkfire", 
                        actionGameActions  =[AddTag  "Already at the altar"]
                    },

                    InteractionAction{
                        actionCondition = TagExist  "Already at the altar",
                        actionDescription = "I'm here now\n", 
                        actionGameActions  = []
                    }

                ]

            
            },

            LocationInteraction{
                interactionSentences = [getMeaningfulSentences ["look", "around"]],
                
                interactionActions =[
                    InteractionAction{
                        actionCondition = GameAnd (TagExist  "Already at the altar") (GameNot (YouAlreadyHaveThisItem "stormbridge sword")),
                        actionDescription = "Near the altar there is a mound with a mysterious sword called the stormbridge sword.", 
                        actionGameActions  =[]
                    },

                    InteractionAction{
                        actionCondition = GameAnd (TagExist  "Already at the altar") (YouAlreadyHaveThisItem "stormbridge sword"),
                        actionDescription = "I don't see anything relevant", 
                        actionGameActions  = []
                    }

                ]

            
            },
            LocationInteraction{
                interactionSentences = [getMeaningfulSentences ["go to", "blood portal"]],
                
                interactionActions =[
                    InteractionAction{
                        actionCondition = GameNot(TagExist "Already at the blood portal") ,
                        actionDescription = "The traveler walked to the blood portal...\n",
                        actionGameActions  =[]
                    },
                    InteractionAction{
                        actionCondition = GameNot(TagExist "molotov exploted") ,
                        actionDescription = "A strange sound is heard when suddenly ...\n"++"A mysterious skull-busting molotov cocktail explodes\nBUMM\n\n"++
                        "The traveler falls to the ground as a result of the impact and is seriously injured.\n Can't get up...\n"++
                        "", 
                        actionGameActions  = [RDamage 80,AddTag "molotov exploted"]
                        
                        
                    }
                    -- InteractionAction{
                    --     actionCondition = GameNot(TagExist "Already at the blood portal") ,
                    --     actionDescription = "The traveler walked to the blood portal", 
                    --     actionGameActions  =[]
                    -- },
                    
                ]

            
            }
            
        
            
        ]
    }



createWorld :: Data.Map.Map String Location-> [String] -> Location -> Player  -> World
createWorld locations ends defaulRoom player= 
    World{locations = locations,
    player = player,
    tags = ["game started"],
    endGames = ends, 
    communActions = defaulRoom
    }

-- Inicializacions
-- -----------------------------------------------------------------
gameWorld :: Player -> World
gameWorld = createWorld  rooms ends  commonActions     
        where (rooms, ends) = locationsMap


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
        bag= ["potion"]
    }

-- ----------------------------------------------------------------

locationsMap :: (Map [Char] Location , [[Char]])
locationsMap = (Data.Map.fromList [("azukiarai",azukiarai),("casentinesi", casentinesi)], ["todavia"])