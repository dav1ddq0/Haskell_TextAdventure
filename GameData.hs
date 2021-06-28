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
    Verb "help" ["help"],
    Verb "exit" ["exit"],
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
    Verb "leave" ["leave"],
    Verb "attack" ["attack"],
    Verb "who Iam"["who Iam","who iam"]
    ]

nouns :: [Token]
nouns = [
    Noun "fire" ["fire"],
    Noun "north" ["north"],
    Noun "south" ["south"],
    Noun "east" ["east"],
    Noun "key" ["key"],
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
    Noun "blood portal" ["blood portal"],
    Noun "Xerneas spectrum" ["Xerneas spectrum"],
    Noun "boss" ["boss"],
    Noun "items"["items"],
    Noun "[rhydon drill, magic stick]" ["[rhydon drill, magic stick]","[magic stick, rhydon drill]"]
    ]

prepositions :: [Token]
prepositions =[
    Preposition "in" ["in", "inside", "within"],
    Preposition "into" ["into"],
    Preposition "the" ["the"],
    Preposition "with" ["with"],
    Preposition "to" ["with"]
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
                        actionCondition = GameAnd(GameNot(TagExist "mantra fusion used"))(GameNot (YouAlreadyHaveThisItem "rhydon drill")),
                        actionDescription = "There is a strange rhydon drill object near the cave mosses.\n",
                        actionGameActions  =[]
                    },
                    InteractionAction{
                        actionCondition = GameAnd (GameNot(TagExist "mantra fusion used")) (GameNot (YouAlreadyHaveThisItem "magic stick")),
                        actionDescription = "There is a magic stick near the lava river.\n",
                        actionGameActions = []
                    },
                    InteractionAction{
                        actionCondition =  GameNot (TagExist  "rock moved"), 
                        actionDescription = "There is a big rock blocking the exit\n",
                        actionGameActions  =  []
                    },
                    InteractionAction{
                        actionCondition =  GameTrue, 
                        actionDescription = "An old spectrum named Xerneas is seen.\n",
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
                        actionCondition = GameAnd (GameNot (YouAlreadyHaveThisItem "rhydon drill"))(GameNot(TagExist "mantra fusion used")),
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
                        actionCondition = GameAnd(GameNot (YouAlreadyHaveThisItem "magic stick"))(GameNot(TagExist "mantra fusion used")),
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
                        actionGameActions  =[AddTag "mantra fusion used",AddItemToBag "rockbreaker", RemoveItemFromBag "rhydon drill", RemoveItemFromBag "magic stick"]
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
                    },

                    InteractionAction{
                        actionCondition = LowHealth,
                        actionDescription = "Can't get up...\n", 
                        actionGameActions  = []
                    }


                ]

            
            },

            LocationInteraction{
                interactionSentences = [getMeaningfulSentences ["take", "stormbridge sword"]],
                
                interactionActions =[
                    InteractionAction{
                        actionCondition = GameAnd(GameNot(YouAlreadyHaveThisItem "stormbridge sword"))(TagExist  "Already at the altar"),
                        actionDescription = "stormbridge sword was added to the bag\n", 
                        actionGameActions  =[AddItemToBag "stormbridge sword"]
                    },

                    InteractionAction{
                        actionCondition = GameAnd(YouAlreadyHaveThisItem "stormbridge sword")(TagExist  "Already at the altar"),
                        actionDescription = "This object has already been taken\n", 
                        actionGameActions  = []
                    },
                    InteractionAction{
                        actionCondition = GameNot(TagExist  "Already at the altar"),
                        actionDescription = "There is no object with that name here\n", 
                        actionGameActions  = []
                    },
                    InteractionAction{
                        actionCondition = LowHealth,
                        actionDescription = "Can't get up...\n", 
                        actionGameActions  = []
                    }


                ]

            
            },

            LocationInteraction{
                interactionSentences = [getMeaningfulSentences ["look", "around"]],
                
                interactionActions =[
                    InteractionAction{
                        actionCondition = GameAnd (TagExist  "Already at the altar") (GameNot (YouAlreadyHaveThisItem "stormbridge sword")),
                        actionDescription = "Near the altar  there is a mound with a mysterious sword called the stormbridge sword.", 
                        actionGameActions  =[]
                    },

                    InteractionAction{
                        actionCondition = TagExist  "Already at the altar",
                        actionDescription = "There is a mysterious knight named Blood Death Knight. Maybe you can talk to him", 
                        actionGameActions  = []
                    },
                    InteractionAction{
                        actionCondition = GameAnd (GameNot(TagExist  "Already at the altar")) (GameNot (TagExist "Already at the blood portal")),
                        actionDescription = "Nearby there is a mysterious altar called Altar of Elders Darkfire\n"++
                        "Nearby is a mysterious portal called Blood Portal\n"++ "On the floor there are some items that look like potions.\n You can write take items to collect them"++
                        "", 
                        actionGameActions  = []
                    }

                ]

            
            },
            LocationInteraction{
                interactionSentences = [getMeaningfulSentences ["speak", "with","Blood Death Knight"]],
                
                interactionActions =[
                    InteractionAction{
                        actionCondition = GameAnd (TagExist  "Already at the altar") (GameNot (TagExist "You already talked to the knight")),
                        actionDescription = "Welcome young traveler.\n I give you the blessing of a new element the fire ...\n" ++
                        "You got the fireball power", 
                        actionGameActions  =[AddTag "fireball",AddTag"You already talked to the knight"]
                    },

                    InteractionAction{
                        actionCondition = GameAnd (TagExist  "Already at the altar") (TagExist "You already talked to the knight"),
                        actionDescription = "Use this valiant traveling power well. It will help you clear some areas", 
                        actionGameActions  = []
                    },
                    InteractionAction{
                        actionCondition = GameNot (TagExist  "Already at the altar"),
                        actionDescription = "No one to talk to", 
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
                        actionGameActions  =[RemoveTag "Already at the altar",RemoveTag "Already at the greengrass woods", AddTag "Already at the blood portal"]
                    },
                    InteractionAction{
                        actionCondition = GameAnd (GameNot(TagExist "Already at the blood portal")) (GameNot(TagExist "molotov exploted")),
                        actionDescription = "A strange sound is heard when suddenly ...\n"++"A mysterious skull-busting molotov cocktail explodes\nBUMM\n\n"++
                        "The traveler falls to the ground as a result of the impact and is seriously injured.\n Can't get up...\n"++
                        "A strange fire goblin appears and steals the exodar teleport\nYou still can't move", 
                        actionGameActions  = [RDamage 88,AddTag "molotov exploted"]
                        
                        
                    },
                    InteractionAction{
                        actionCondition = GameAnd (GameNot(TagExist "Already at the blood portal")) (TagExist "molotov exploted"),
                        actionDescription = "Remains of the explosion can be seen near the portal\n", 
                        actionGameActions  = []
                        
                        
                    }
                    -- InteractionAction{
                    --     actionCondition = GameNot(TagExist "Already at the blood portal") ,
                    --     actionDescription = "The traveler walked to the blood portal", 
                    --     actionGameActions  =[]
                    -- },
                    
                ]

            
            },
            LocationInteraction{
                interactionSentences = [getMeaningfulSentences ["go to", "greengrass woods"]],
                
                interactionActions =[
                    InteractionAction{
                        actionCondition = GameAnd (GameNot LowHealth)(GameNot(TagExist "Already at greengrass woods")) ,
                        actionDescription = "The traveler walked to the greengrass woods...\n",
                        actionGameActions  =[RemoveTag "Already at the altar",RemoveTag "Already at the blood portal", AddTag "Already at the greengrass woods"]
                    },
                    InteractionAction{
                        actionCondition = LowHealth,
                        actionDescription = "You can't move because of the impact\n",
                        actionGameActions  =[RemoveTag "Already at the altar",RemoveTag "Already at the blood portal", AddTag "Already at the greengrass woods"]
                    },
                    InteractionAction{
                        actionCondition = GameNot(TagExist "clear grass") ,
                        actionDescription = "You can hardly see anything, the grass covers everything...\n",
                        actionGameActions  =[]
                    }
                    
                ]

            
            },
            LocationInteraction{
                interactionSentences = [getMeaningfulSentences ["use", "fireball"]],
                
                interactionActions =[
                    InteractionAction{
                        actionCondition = GameAnd(GameAnd(TagExist "Already at greengrass woods")(GameNot(TagExist "clear grass"))) (TagExist "fireball power") ,
                        actionDescription = "A great ball of fire was shot\n",
                        actionGameActions  =[AddTag "clear glass"]
                    },
                    InteractionAction{
                        actionCondition = GameAnd(TagExist "Already at greengrass woods")(GameNot(TagExist "clear grass")) ,
                        actionDescription = "You can hardly see anything, the grass covers everything...\n"++
                        "Suddenly the same goblin that had stolen the teleport appears\n",
                        actionGameActions  =[AddTag "clear grass",AddTag "goblin appear"]
                    },
                    InteractionAction{
                        actionCondition = GameAnd(TagExist "Already at greengrass woods")(TagExist "clear grass") ,
                        actionDescription = "The grass has already been cleared, it is not necessary to use it\n",
                        actionGameActions  =[]
                    },
                    InteractionAction{
                        actionCondition = GameAnd(TagExist "Already at greengrass woods")(TagExist "clear grass") ,
                        actionDescription = "The grass has already been cleared, it is not necessary to use it\n",
                        actionGameActions  =[]
                    },
                    InteractionAction{
                        actionCondition = GameNot(TagExist "Already at greengrass woods"),
                        actionDescription = "You shouldn't use that power here\n",
                        actionGameActions  =[]
                    }
                    
                ]

            
            },
            LocationInteraction{
                interactionSentences = [getMeaningfulSentences ["attack", "globlin"]],
                
                interactionActions =[
                    InteractionAction{
                        actionCondition = GameAnd (TagExist "Already at greengrass woods")(GameAnd(YouAlreadyHaveThisItem "stormbridge sword")(TagExist "goblin appear")),
                        actionDescription = "The traveler attacks the globlin with his sword... \n"++
                        "The goblin dies ...\n. When he dies he drops a bag with various items\n",
                        actionGameActions  =[RemoveTag "goblin appear",AddTag "There are items on the greengrass floor"]
                    },
                    InteractionAction{
                        actionCondition = GameOr(TagExist "globlin appear")(GameNot (TagExist "Already at greengrass woods")) ,
                        actionDescription = "There is no one to attack...\n",
                        actionGameActions  =[]
                    },
                    InteractionAction{
                        actionCondition = GameAnd(TagExist "globlin appear")(GameAnd(TagExist "Already at greengrass woods")(GameNot(YouAlreadyHaveThisItem "stormbridge sword"))),
                        actionDescription = "You have nothing to attack with.\n The globin laughs and hits you black blood...\n",
                        actionGameActions  =[RDamage 50]
                    }
                    
                    
                ]

            
            },
            LocationInteraction{
                interactionSentences = [getMeaningfulSentences ["get", "items"]],
                
                interactionActions =[
                    InteractionAction{
                        actionCondition = GameAnd (TagExist  "There are items on the greengrass floor") (TagExist "Already at greengrass woods"),
                        actionDescription = "You have taken the following items:...\n"++
                        "2potions,1 energy drink and the the ancestral *Teleport Exodar*",
                        actionGameActions  =[RemoveTag "There are items on the greengrass floor", AddItemToBag "Potion", AddItemToBag "Potion", AddItemToBag "energy drink", 
                        AddItemToBag "teleport exodar"]
                    },
                    InteractionAction{
                        actionCondition = GameOr (GameNot(TagExist "Already at greengrass woods")) (TagExist "There are items on the greengrass floor"),
                        actionDescription = "There is nothing to take...\n",
                        actionGameActions  =[]
                    }
                    
                ]

            
            },
            LocationInteraction{
                interactionSentences = [getMeaningfulSentences ["activate", "teleport"]],
                
                interactionActions =[
                    InteractionAction{
                        actionCondition = GameAnd (TagExist "Already at the blood portal") (YouAlreadyHaveThisItem "activate"),
                        actionDescription = "Suddenly a sphere of energy opens that transports towards the inhospitable Night's dawn",
                        actionGameActions  =[AddTag  "sphere open"]
                    },
                    InteractionAction{
                        actionCondition = GameOr (GameNot(TagExist "Already at the blood portal")) (GameNot (YouAlreadyHaveThisItem "teleport exodar")),
                        actionDescription = "What are you trying to do? There is nothing to activate",
                        actionGameActions  =[]
                    }
                    
                ]

            
            },
            LocationInteraction{
                interactionSentences = [getMeaningfulSentences ["enter", "the" ,"sphere"]],
                
                interactionActions =[
                    InteractionAction{
                        actionCondition = GameAnd (TagExist "Already at the blood portal") (TagExist "sphere open"),
                        actionDescription = "You have entered the sphere of transportation ...A new destination awaits you",
                        actionGameActions  =[RemoveTag "sphere open",RemoveTag "Already at the blood portal",NextLocation "Night's dawn"]
                    },
                    InteractionAction{
                        actionCondition = GameOr (GameNot(TagExist "Already at the blood portal")) (GameNot (TagExist "sphere open")),
                        actionDescription = "There is no transportation sphere in sight\n",
                        actionGameActions  =[]
                    }
                    
                ]

            
            }


            
        
            
        ]
    }

nightsDawn :: Location
nightsDawn= Location{
    locationId ="Night's dawn",
    locationDescription = "**NIGHT'S DAWN**\n"++
    "Where darkness takes over life and ice runs through the landscape.\n" ++
    "The place is very dark, you can hardly see anything. ...\n" ++
    "I have stumbled upon something let me catch it.\n",
    locationInteractions =[
        LocationInteraction {
                interactionSentences = [getMeaningfulSentences ["look", "around"]],
                interactionActions =[
                    InteractionAction {
                        actionCondition = GameNot(TagExist "light on") ,
                        actionDescription  ="I see almost nothing.\n I only feel a strange object near my foot\n",
                        actionGameActions  = []
                    },
                    InteractionAction {
                        actionCondition = TagExist "light on",
                        actionDescription  ="There are three fire containers.\n Maybe you can light them with the torch\n",
                        actionGameActions  = []
                    }

                ]
            },
        LocationInteraction {
                interactionSentences = [getMeaningfulSentences ["get", "unknow item"]],
                interactionActions =[
                    InteractionAction {
                        actionCondition = YouAlreadyHaveThisItem "torch" ,
                        actionDescription  = "No nothing to catch\n",
                        actionGameActions  = []
                    },
                    InteractionAction {
                        actionCondition = GameNot(YouAlreadyHaveThisItem "torch"),
                        actionDescription  ="Ohh a torch.\n The torch was added to the bag\n",
                        actionGameActions  = [AddItemToBag "torch"]
                    }
                    
                ]
            },
        LocationInteraction {
                interactionSentences = [getMeaningfulSentences ["use", "fireball"]],
                interactionActions =[
                    InteractionAction {
                        actionCondition = GameAnd(YouAlreadyHaveThisItem "torch")(GameNot(TagExist "burning torch")) ,
                        actionDescription  = "You have lit the torch.\n... Now you can see better inside so much darkness\n",
                        actionGameActions  = [AddTag "light on"]
                    },
                    InteractionAction {
                        actionCondition = GameNot(YouAlreadyHaveThisItem "torch"),
                        actionDescription  ="There is nothing to set fire\n",
                        actionGameActions  = []
                    },
                    InteractionAction {
                        actionCondition = GameAnd(TagExist "Already at fire container1")(GameNot(TagExist "container1 on")),
                        actionDescription  ="You used fireball power to light the container1\n",
                        actionGameActions  = [AddTag "container1 on"]
                    },
                    InteractionAction {
                        actionCondition = GameAnd(TagExist "Already at fire container1")(TagExist "container1 on"),
                        actionDescription  ="This container is already lit\n",
                        actionGameActions  = []
                    },
                    InteractionAction {
                        actionCondition = GameAnd(TagExist "Already at fire container2")(GameAnd(TagExist "container1 on")(GameNot(TagExist "container2 on"))),
                        actionDescription  ="You used fireball power to light the container2\n",
                        actionGameActions  = [AddTag "container2 on"]
                    },
                    InteractionAction {
                        actionCondition = GameAnd(TagExist "Already at fire container2")(TagExist "container2 on"),
                        actionDescription  ="This container is already lit\n",
                        actionGameActions  = []
                    },
                    InteractionAction {
                        actionCondition = GameAnd(GameAnd(TagExist "Already at fire container2") (GameNot(TagExist "container1 on")))(GameNot(TagExist "container2 on")),
                        actionDescription  ="Container 2 can only be turned on after Container 1\n",
                        actionGameActions  = []
                    },
                    InteractionAction {
                        actionCondition = GameAnd(TagExist "Already at fire container3")(GameAnd(TagExist "container2 on")(GameNot(TagExist "container3 on"))),
                        actionDescription  ="You used fireball power to light the container3\n" ++
                        "The ritual is completed now a strange being emerges ...\n"++
                        "Gate Guardian Appears\n",
                        actionGameActions  = [AddTag "container3 on", AddTag "gate guardian"]
                    },
                    InteractionAction {
                        actionCondition = GameAnd(TagExist "Already at fire container3")(TagExist "container3 on"),
                        actionDescription  = "This container is already lit\n",
                        actionGameActions  = []
                    },
                    InteractionAction {
                        actionCondition = GameAnd(TagExist "Already at fire container3")(GameAnd(GameNot(TagExist "container3 on"))(TagExist "container2 on")),
                        actionDescription  = "Container 3 can only be turned on after Container 2\n",
                        actionGameActions  = []
                    }
                    
                ]
            },
        LocationInteraction {
                interactionSentences = [getMeaningfulSentences ["go to", "fire container1"]],
                interactionActions =[
                    InteractionAction {
                        actionCondition = GameNot(TagExist "Already at fire container1"),
                        actionDescription  = "You have moved to Container 1\n",
                        actionGameActions  = [AddTag "Already at fire container1",RemoveTag "Already at fire container2",
                        RemoveTag "Already at fire container3"]
                    },
                    InteractionAction {
                        actionCondition = TagExist "Already at fire container1",
                        actionDescription  = "You are already in fire container 1\n",
                        actionGameActions  = []
                    }
                    
                ]
            },
        LocationInteraction {
                interactionSentences = [getMeaningfulSentences ["go to", "fire container2"]],
                interactionActions =[
                    InteractionAction {
                        actionCondition = GameNot(TagExist "Already at fire container2"),
                        actionDescription  = "You have moved to Container 2\n",
                        actionGameActions  = [AddTag "Already at fire container2",RemoveTag "Already at fire container2",
                        RemoveTag "Already at fire container3"]
                    },
                    InteractionAction {
                        actionCondition = TagExist "Already at fire container2",
                        actionDescription  = "You are already in fire container 2\n",
                        actionGameActions  = []
                    }
                    
                ]
            },
        LocationInteraction {
                interactionSentences = [getMeaningfulSentences ["go to", "fire container3"]],
                interactionActions =[
                    InteractionAction {
                        actionCondition = GameNot(TagExist "Already at fire container3") ,
                        actionDescription  = "You have moved to Container 3\n",
                        actionGameActions  = [AddTag "Already at fire container3",RemoveTag "Already at fire container1",
                        RemoveTag "Already at fire container2"]
                    },
                    InteractionAction {
                        actionCondition = TagExist "Already at fire container3",
                        actionDescription  = "You are already in fire container 3\n",
                        actionGameActions  = []
                    }
                    
                ]
            },
        LocationInteraction {
                interactionSentences = [getMeaningfulSentences ["open", "dungeon gate"]],
                interactionActions =[
                    InteractionAction {
                        actionCondition = YouAlreadyHaveThisItem "key" ,
                        actionDescription  = "You have moved to Container 3\n",
                        actionGameActions  = [RemoveTag  "Already at fire container3",RemoveTag "Already at fire container1",
                        RemoveTag "Already at fire container2", NextLocation "bot"]
                    },
                    InteractionAction {
                        actionCondition = TagExist "Already at fire container3",
                        actionDescription  = "You are already in fire container 3\n",
                        actionGameActions  = []
                    }
                    
                ]
        },
        LocationInteraction {
                interactionSentences = [getMeaningfulSentences ["talk", "to", "the gate guardian"]],
                interactionActions =[
                    InteractionAction {
                        actionCondition = GameAnd(TagExist "Already at fire container3")(TagExist "gate guardian") ,
                        actionDescription  = "Welcome young traveler behind the door awaits you the final test to defeat the mythical Xavius\n"++
                        "Here I give you the entry key and a new ice wall power",
                        actionGameActions  = [AddTag "dungeon entrance",AddTag  "ice wall",AddItemToBag "key"]
                    },
                    InteractionAction {
                        actionCondition = GameNot(GameAnd(TagExist "Already at fire container3")(TagExist "gate guardian")),
                        actionDescription  = "There is no guardian to talk to\n",
                        actionGameActions  = []
                    }
                    
                ]
            }

    ]
}


dungeonBoss :: Location
dungeonBoss= Location{
    locationId ="Xavius",
    locationDescription = "**XAVIUS BOSS***\nTraveler dare to confront me. ha ha.",
    locationInteractions =
        [
            LocationInteraction {
                interactionSentences = [getMeaningfulSentences ["attack", "boss"]],
                interactionActions =[
                    InteractionAction {
                        actionCondition = GameAnd(GameNot(TagExist "Second attack"))(TagExist "First attack"),
                        actionDescription  = "You attacked Xavius with your sword\n"++
                        "Xavius is still on his feet and launches a wildfire attack at you\n",
                        actionGameActions  = [RDamage 70, AddTag "Second attack"]
                    },
                    InteractionAction {
                        actionCondition = GameNot(TagExist "First attack"),
                        actionDescription  = "Attacked Xavius with your sword using the double-edged spin ability\n"++
                        "Xavius is still standing but more weakened.\nFire spin spear\n",
                        actionGameActions  = [RDamage 50,AddTag "First attack"]
                    },
                    InteractionAction {
                        actionCondition = TagExist "Second attack",
                        actionDescription  = "You attacked Xavius with the sword the thousand-bladed sword ability.\n" ++
                    "Congratulations traveler or you have won the challenge\n"++
                    ".................\n"++
                    "Game Completed :)\n\n",
                        actionGameActions  = [NextLocation "finished"]
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
                interactionSentences = [getMeaningfulSentences ["use","energy drink"]],
                interactionActions =[
                    InteractionAction{
                        actionCondition = YouAlreadyHaveThisItem "energy drink",
                        actionDescription  = "The traveler used a energy drink"  ,
                        actionGameActions  = [RemoveItemFromBag "energy drink", RecoveryEnergy 60]
                    },
                    InteractionAction{
                        actionCondition = GameNot(YouAlreadyHaveThisItem "energy drink"),
                        actionDescription  = "You have no energy drink left in your bag"  ,
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
                    
                
            },
            LocationInteraction {
                interactionSentences = [getMeaningfulSentences ["who Iam"]],
                interactionActions =[
                    InteractionAction{
                        actionCondition = GameTrue,
                        actionDescription  = ""  ,
                        actionGameActions  = [PName]
                    }
                    
                ]
                    
                
            },
            LocationInteraction {
                interactionSentences = [getMeaningfulSentences ["help"]],
                interactionActions =[
                    InteractionAction{
                        actionCondition = GameTrue ,
                        actionDescription  = ""  ,
                        actionGameActions  = [Help]
                    }
                    
                ]
                    
                
            },
            LocationInteraction {
                interactionSentences = [getMeaningfulSentences ["exit"]],
                interactionActions =[
                    InteractionAction{
                        actionCondition = GameTrue ,
                        actionDescription  = ""  ,
                        actionGameActions  = [Exit]
                    }
                    
                ]
                    
                
            }


            
        ]
}
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Game initializations
-- ------------------------------------------------------------------------------------
createWorld :: Data.Map.Map String Location-> [String] -> Location -> Player  -> World
createWorld locations ends defaulRoom player= 
    World{locations = locations,
    player = player,
    tags = ["game started"],
    endGames = ends, 
    communActions = defaulRoom
    }


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
        bag= ["potion", "potion","energy drink"]
    }

-- ----------------------------------------------------------------

locationsMap :: (Map [Char] Location , [[Char]])
locationsMap = (Data.Map.fromList [("azukiarai",azukiarai),("casentinesi", casentinesi), ("Night's dawn", nightsDawn), ("Xavius", dungeonBoss)], ["finished"])