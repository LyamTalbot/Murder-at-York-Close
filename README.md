# Murder at York Close

This is a programming assignment inspired by whodunnits and mystery games.

The game is played by simple computer players (which you will write) and involves elimination and deduction.
It is not turn-based: each player plays simultaneously on each game tick.

## Setting 

Being a tale of misdeed and murder, in the style of the early 1900s, our (fictional) setting is triggered by a letter, received 
by Sir Arthur Conan Doyle after it has been recovered by a survivor of the RMS Titanic.

> To my dear friends at the Society for Psychcical Research,
>
> I awakened this morning
> in my cabin on this most esteemed ship, to find that my gift for automatic writing had been enlivened
> by the gentle passage over the sea. As I struck my pen to the paper, I found my hand guided by a 
> Mrs Elizabeth Dacre. When I gazed at what she had written through me, I saw a dire warning.
> 
> My friends, I urge you to hasten to the manor house of York Close, where foul deeds are planned. One of your
> number is not who you think they are, but a person bent on your destruction. 
>
> Yours sincerely,  
> W T Stead, writing from the RMS Titanic

Sir Arthur is unavailable, being preoccupied with the forthcoming publication of his latest adventure stories 
in The Strand. But an eagre group of amateur psychic investigators (and a hidden villain) has travelled to the
Victorian manor house to investigate...

### How the code is organised

The game runs in the server, with a web client. However, the Ticks are prompted by requests from the client.

* The `ui` module contains the browser code. This isn't intended to be edited. It's clunky, but should be ok.

* The `server` module contains both a *Pekko-HTTP* server and the Actors for the game. 

It uses typed Pekko actors.

### Running the game

The project uses [SBT](https://scala-sbt.org) as its build tool.

If you run `sbt`, it'll take you to an interactive command prompt. (If you need to exit this, Ctrl-D)

The SBT project has two parts:

* The UI, which uses Scala.js but you shouldn't need to edit it or run it directly.
* The server, which runs on the JVM

From the SBT interactive prompt:

> server/run 8080 true

Will compile *both* the client and the server, and start up a server listening on port 8080. The `true` at the end tells it so wait for you to press Enter at the command prompt to *stop* the server again.

If you forget, and have lots of sbt processes you don't know how to get rid of, then from a new terminal 

> sbt shutdownall

Will shutdown any SBT processes it finds.

You can also run it all in one go from a bash terminal, e.g. 

> sbt "server/run 8080 true"

(You might need the quotes around the arguments to do this)

When you have a game server running, open a web browser on the same computer and visit

> `http://localhost:8080` (or whatever alternative port number you chose)

The server will deliver the HTML and compiled client to your browser. It'll also open a channel to listen to messages from your server (using Server Sent Events).

Note: There is a "filthy hack" left in the code, whereby if you open a second browser, the first one will stop hearing events. You might get given a task to fix this - it depends what tasks your assessment sets.

### The game mechanics

* The game runs on a tick timer, with all players taking action once per tick. This tick is announced by all players receiving a `TurnUpdate` message.

* The manor house is laid out in a tiled map. Each character can move one square N, S, E, or W on each tick.

* There are various locations in the house and grounds, including the Conservatory, Dining Room, etc, but also Gardens and a Boat House.
  The list of locations can be found in `house.scala`

* The players arrive knowing (from W T Stead's letter) that one of them is a murderer, but they do not know which.

* On any tick, each player can see all other players who are in the same room as they are. (Unless they are in a doorway in which case they
  are too distracted opening and closing the door to notice who is near them.)

* Around the grounds are various deadly weapons. If the murderer is alone in a location with another player and a weapon is present, they will strike!

* The murderer will also dispose of the weapon. The only way to know what weapons have been used is by their disappearance from a location.

* When someone is murdered, a blood-curdling scream will emanate across the grounds. Players will know from the voice who the poor victim is, but not
  where it took place.

* When a scream is heard, anyone the players can see cannot be the murderer. 
  (As the only person in the same room as the murderer is no longer able to bear witness to it.)

* The ghost of Elizabeth Dacre also inhabits the house. When a player knows the murderer, at least one victim, at least one used weapon, and at least one used location,
  they can call out to her ghost (send the gameActor an `ElizabethDacreCommand`) their accusation. **Note that you don't have to know which victim was killed with which weapon or in which location - you just need to know one of the victims, one of the weapons used, one of the locations, and the murderer**.

* If a player knows who the murderer, at least one weapon, and at least one victim are, and they are alone
  in a room, they can announce it to the ghost, winning the game. If their accusation is wrong, the game is lost.

### Your tasks

This starter code is designed so that a teacher can set you questions about it and tasks to do with it (e.g. getting the players to identify the murderer).
How those questions are set depends on your teacher's quiz-setting environment.

