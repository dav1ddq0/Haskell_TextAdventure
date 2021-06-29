

### **Proyecto de Haskell**

### **Programación Declarativa**

Integrantes:

### **David Orlando De Quesada Oliva C311**

### **Javier Domínguez C312**

### **Daniel de la Cruz C311**









Version de Hasell usada:

```
The Glorious Glasgow Haskell Compilation System, version 8.6.5
```



Módulos usados :

```
Data.Map
System.IO
Data.Time.Clock
Data.List
Data.Char
System.Exit
Data.List.Split
Data.Time.LocalTime
Data.Time
```

El proyecto esta dividido en 7 files .hs 

```
MainGame.hs
GameProcessing.hs
GameParser.hs
GameModeling.hs
GameLexer.hs
GameUtils.hs
GameData.hs
```



en GameModeling.hs está definida la idea general de como decidimos hacer el text adventure

Usando las características que tiene el leguaje haskell para definir nuevos tipos de datos usando records  lo que permite extraer luego el campo que se necesite.

Lo más general del juego lo definimos con World :

```
data World = World{
    locations :: Data.Map.Map String Location,
    player :: Player,
    tags :: Tags,
    endGames :: [String],
    communActions :: Location
}deriving (Show, Eq)
```

El juego está formado por un conjunto de locations que representa cada escenario del juego. De la forma que está hecho es bastante extensible pues definiendo nuevos locations e interconectándolos con los ya existenten se puede ampliar y añadir nuevas opciones a la historia. Además tiene un jugador, una lista de etiquetas , una listas de finales y un location especial que sirve para definir las acciones generales que se hacen en el juego sin definir en que location esté el jugador actualmente. Las etiquetas son muy útiles pues sirven para definir que cosas han pasado en el juego, como a donde se ha ido  etc, con estás se puede saber  también si el jugador ya consiguió determinado objetivo como hablar con un héroe etc. 

Un jugador está definido como:

```
data Player = Player {
    playerName ::String,
    playerLife :: Int,
    playerMagic :: Int,
    bag:: Bag

}deriving(Show,Eq)
```

 Tiene un nombre, se define con el que se escriba en la consola, una vida y magia que inicialmente valen 100 y siempre tienen un valor entre 0 y 100.

El player también posee un inventario en el cual van a estar los items que vaya cogiendo en la travesía

Una location se define:

```
data Location = Location {
    locationId :: String,
    locationDescription :: String,
    locationInteractions :: [LocationInteraction]
} deriving(Show,Eq)
```

Tiene un id , una descripción la cual se imprime en pantalla  y una listas de interacciones las cuales machearán o no con lo que escriba el usuario

Con el tipo GameCondition definimos todos las posibles condicionales que se usarán en el juego

Esto permite definir cuando

```
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
```

Estas condicionales se verfican en `GameProcessing.hs` con el método  evalCondition usando pattern matching

```
-- -------------------------------------------------------------------------------
-- Evaluar la condicion para saber si se cumple la interacción
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
```

Por ejemplo si quiero verificar si ya hablé con Xerneas puedo poner:

```
GameNot (TagExist  "You already spoke with Xerneas")
```

Game Condition fue definido haciendo uso de los tipos recursivos por lo que se le puede agregar fácilmente nuevas condicionales para alguna característica nueva que se le quiera agregar al juego lo que lo hace extensible.



GameAction en GameModeling.hs define todas las acciones del juego como Add Tag que permite a añadir una nueva etiqueta a la lista de etiquetas .

GameAction está definido usando la definición de tipos de datos recursivos

```
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
```

Una vez que matchee un InteractionAction se manda a ejecutar cada GameAction definido en este

A GameAction también se le pueden agregar fácilmente nuevas acciones que se quieran agregar al juego dada su definición.



Con exeGameAction en GameProcessing.hs ejecutamos cada GameAction de la lista de GameActions que tiene el InteractionAction que matcheó

LocationInteraction  está definido por una listas de Sentence  y una lista de InteractioAction .La lista de Sentence es lo que se compara con la Sentence que devuelve el parseo de lo que se escriba en consola si la Sentence producto de parsear la línea de entrada coincide con algunas de las que tiene la lista entonces esa LocationInteraction matchea y se revisa en la listas de interactionActions cuales se cumplen

```
data LocationInteraction = LocationInteraction {
    interactionSentences :: [Sentence],
    interactionActions :: [InteractionAction] 
}deriving (Show,Eq)
```

 InteractionAction 

```
data InteractionAction = InteractionAction{
    actionCondition :: GameCondition,
    actionDescription :: String,
    actionGameActions :: [GameAction]
}deriving (Eq, Show)
```

InteractionAction está formado por un actionCondition que es del tipo GameCondition, un actionDescription que describe lo que se mostraría en pantalla en caso que se cumpla el actionCondition y  una lista de GameAction que se ejecutarán en caso que se cumpla el actionCondition.







**Ejecutar el proyecto:**



```
ghci MainGame.hs
```

<img src="/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/1.png" style="zoom: 80%;" />

Luego  **adventure begin**

```
play
```

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/2.png)

Te va a pedir que escribas el nombre que le quieres dar al jugador



![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/3.png)

Luego de ingresar el nombre que le quieras dar al usuario te va a imprimir la historia general del juego basado en el mundo ficticio de Fireblood.

Y aparece el jugador en el lugar inicial que es `Azukiarai Cave`. Se imprime cada vez que se cambie de lugar una breve descripción del mismo.

Para saber que puedes hacer o a donde puedes ir type `look around` , esto te muestra alguna información relevante que te servirá para ir avanzando en la historia del juego.

En este lugar al escribir look around inicialmente debe aparecer lo siguiente:

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/4.png)

Por lo que hay dos objetos que podemos tomar y un misterioso spectrum con el que quizás puedas hablar

En todo momento del juego puedes escriber lo siguiente:

```
view status
```

`Muestra el estado del jugador en el momento actual`

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/5.png)



```
view bag
```

`Muestra los items que tiene el jugador en su bolsa.`

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/6.png)

```
who Iam
```

`Imprime en pantalla el nombre del jugador`

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/7.png)



```
give me the time
```

`Te muestra en pantalla la hora actual del juego que concidirá con la que tengas en la PC`



![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/8.png)



```
use potion
```

`Permite en caso que el jugador disponga de pociones para la vidad en su bolsa recuperar un porciento de vida perdida`

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/9.png)

```
use energy drink
```

`Permite en caso que el jugador disponga de bebidas energizantes en su bolsa recuperar un porciento de magia perdida`

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/10.png)

```
help
```

`Imprime en pantalla algunas acciones generales del juego que se pueden hacer`

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/11.png)

```
exit
```

`Fuerza la salida del juego`

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/12.png)

Ahora según lo que mostró `look around` la primera vez que fue escrito hay dos objetos podemos tomar `rhydon drill` y `magic stick`

Se procede a tomar estos dos objetos:

```
take rhydon drill
```

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/13.png)



```
take magic stick
```

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/14.png)

Una vez tomado estos dos objetos podemos ver que fueron agregados correctamente a la bolsa del jugador si escribimos `view bag` lo que debe mostrar lo siguiente

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/15.png)

Ahora debemos ir a donde está el spectrum de Xerneas para que nos de un misterioso poder

Para eso escribimos:

```
go to Xerneas spectrum
```

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/16.png)

Al ir a ver a Xerneas este nos da un nuevo poder llamado mantra fusion que nos va a servir para combinar items y obtener otros.

Para poder salir de la cueva donde se encuentra el jugador es necesario destruir una gran roca que bloquea la salida

Procedamos  a usar el nuevo poder que nos fue dado

Escribimos:

```
use mantra fusion
```

 ![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/17.png)

Ahora con la fusión de `rhydon drill` y `magic stick` has obtenido un nuevo objeto `rockbreaker` que te va a servir para poder desbloquear la salida.

Si ahora revisamos la bolsa escribiendo `view bag`

 ![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/18.png)



Vemos como ya no aparecen los dos items que fueron fusionados y ahora está rockbreaker

Ahora para romper lo que bloquea la salida

Escribimos `break big rock` para romper la gran roca usando el item rockbreaker

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/19.png)

Si volvemos a escribir  `look around` vemos que nos muestra que ya la salida fue despejada

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/20.png)

Para salir de la cueva escribimos `leave the cave`

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/21.png)

Al salir de la cueva llegamos  a un nuevo lugar `Casentinesi Blood Forest` . Se imprime una breve descripción de este nuevo lugar.

Se escribimos `look around ` nos imprime lo siguiente:



![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/24.png)



Como dice la descripción hay 3 lugares a los que podemos ir. Para moverte a los mismos escribe `go to blood portal`,`go to greengrass woods  ` y `go to altar of Elders Darkfire ` respectivamente



Si vamos primero al `altar of Elders Darkfire` escribimos go to altar of Elders Darkfire y nos imprime en pantalla lo siguiente:

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/25.png)

Si escribimos `look around` nos mostrará información relevante del lugar:

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/26.png)

Según la información podemos tomar una espada y hablar con un misterioso caballero

Para tomar la espada escribimos:

```
take stormbridge sword
```

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/27.png)

Esta espada será necesario para los combates de los contrario estarás desprotegido ante cualquier amenaza

Para hablar con el misterioso caballero escribimos:

```
speak with Blood Death Knight
```

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/29.png)

Al hablar con el caballero este te dará el poder de fireball que será necesario en la travesía



Luego que obtengamos el poder fireball y la espada no hay más nada que hacer cerca del altar

Ahora nos movemos a Blood Portal para eso escribimos:

```
go to blood portal
```

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/30.png)

La primera vez que se mueva el viajero al Blood Portal será detonada una bomba que dejará al viajero con muy poca vida incapaz de moverse.

Será necesario que uses una poción para que recupere vida y pueda continuar con la travesía

Si escribes 

```
view status
```

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/31.png)

Podrás ver que en efecto la vida el player está muy baja

Ahora usamos una poción permita recuperar 60 de vida

Escribes :

```
use potion
```

y al volver a revisar el estado vemos que recuperó un poco de vida

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/33.png)

Ahora que el usuario recuperó un poco de vida se puede mover

Ahora nos movemos a Greengrass Woods, para eso escribimos:

```
go to greengrass woods
```

Este lugar está cubierto por hierba alta y no se ve casi nada

Ahora que disponemos de la habilidad fireball la usamos para despejar el lugar

Escribimos

```
use fireball
```

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/34.png)

al despejar el lugar sale el goblin que había robado el teleport 

Será necesario que lo mates usando la espada

Para matar al goblin escribe

```
attack goblin
```

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/36.png)



Recogemos esos items

Pare eso escribimos:

```
take items
```

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/37.png)

Revisamos la bolsa:

```
view bag
```

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/38.png)



Una vez obtenido el teleport vamos de nuevo al Blood Portal

```
go to blood portal
```

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/39.png)

Ahora tenemos que usar el teleport 

Para eso escribimos:

```
activate teleport
```

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/40.png)

Ahora el viajero tienes que entrar a la esfera de transportación 

Para eso escribe:

```
enter the sphere
```



![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/41.png)

La esfera transporta al viajero al desolado y oscuro lugar Night's Dawn 

Ahora escribimos `look around` para que nos brinde alguna pista

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/42.png)

Parece ser que hay un extraño objeto cerca 

Para tomar el objeto escribe

```
take unknow item
```

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/43.png)

El misterioso objeto parece ser una antorcha

Quizás si la prendes podrás ver mejor en tanta oscuridad

Para encender la antorcha usar el poder fireball

```
use fireball
```

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/44.png)



Ahora si escribimos `look around` podemos ver nuevas pistas:

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/45.png)

Ahora se revelan tres contenedores para encender con fuego que parecen una especie de ritual

Los contenedores de fuego deben encenderse en orden 

Vamos a ir al primero

escribimos:

```
go to fire container1
```

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/46.png)



Ahora encendemos el recipiento escribiendo:

```
use fireball
```

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/47.png)

Una vez encendido el primer contenedor vamos al segundo

```
go to fire container2
```

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/51.png)



Y lo encendemos con `use fireball`

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/53.png)

Una vez encendido el contenedor 2 vamos al contenedor 3

```
go to fire container3
```

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/54.png)

Ahora encendemos el contenedor 3

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/56.png)

Una vez encendido el contenedor 3 sale un misterioso guardián que protege la puerta de dungeon.

Debes hablar con el guardián

```
speak with guardian
```

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/60.png)

El guardián te dará la llave de la puerta y un nuevo poder

Ahora abre la puerta usando la llave que te dieron al escribir lo siguiente

```
open dungeon gate
```



![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/67.png)



Esta puerta conduce al reto final que es derrotar  a Xavius

Para derrotar a Xavius debes dar tres golpes con la espada pero cada vez que atacas a Xavius el te lanzará un ataque más poderoso por lo que debes de ir mirando tu vida para no morir

Lanzamos el primer ataque

Type:

```
attack boss
```

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/68.png)



Ahora revisamos la vida

```
view status
```

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/70.png)

Vemos que el jugador está bastante herido, es necesario recuperar vida antes del próximo ataque

Usamos una poción

```
use potion
```

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/90.png)

Ahora lanzamos el segundo ataque 

```
attack boss
```

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/91.png)

Si volvemos a revisar la vida

```
view status
```



![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/92.png)

Vemos que le queda muy poca al viajero podemos recuperar si nos quedan pociones pero como sabemos que el tercer ataque derrota a Xavius y el viajero siempre ataca primer

Vovamos a atacar

```
attack boss
```

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/95.png)

Una vez vencido Xavius has logrado completar el juego