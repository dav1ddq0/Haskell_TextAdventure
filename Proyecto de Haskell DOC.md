Proyecto de Haskell



David Orlando De Quesada Oliva C311

Javier Dominguez C312

Daniel de la Cruz C311









Version de Hasell usada:

```
The Glorious Glasgow Haskell Compilation System, version 8.6.5
```



Ejecutar el proyecto:



```
ghci MainGame.hs
```

<img src="/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/1.png" style="zoom: 80%;" />

Luego 

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

`Permite en caso que el jugador disponga de pociones para la vidad en su bolsa recuperar un porciento de vida perdido`

![](/mnt/048835ED8835DDBC/School/3ro/Programacion Declarativa/Proyecto Haskell/Code/3/Haskell_TextAdventure/Images/9.png)

```
use energy drink
```

`Permite en caso que el jugador dispoga de bebidas energizantes en su bolsa recuperar un porciento de magia perdido`

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

Ahora con la fusión de `rhydon drill` y `magic stick` has obtenido un nuevo objeto `rockbreaker` que te va a servir para poder desbloquear la salida .

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

Según la información podemos tomar una espado y hablar con un misterioso caballero

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





Una vez ejectutado debe de salir la historia principal del text adventure basada en un mundo ficticio  creado llamado "Fireblood" inspirado en algunas historias fant'asticas .



El juego esta estructurado en un conjunto de lugares llamados location que representa cada parte del juego un jugador y todo eso se mete en world

Usamos bastante el pattern matching para el parser 

El juevo tiene un conjunto de etiquetas que describes que acciones han de hacerse luego que se matche con una interaccion