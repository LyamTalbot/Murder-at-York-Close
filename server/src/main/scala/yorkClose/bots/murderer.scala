package yorkClose.bots

import org.apache.pekko
import pekko.actor.typed.*
import pekko.actor.typed.scaladsl.*

import yorkClose.*
import game.*

/**
  * You are the murderer.
  * 
  * Students should not need to edit this file, though it is useful as an example.
  * 
  * The murderer goes randomly from room to room, looking for an opportunity to strike.
  *
  * @param gameActor A reference to the actor running the game, so we can send it our commands.
  * @param player The Player this actor is playing (e.g. Player.Pink)
  * @param location The (x, y) location the player has initially spawned in
  * @return
  */
def murderer(player:Player, location:Location)(using gameActor: ActorRef[GameMessage]):Behavior[Message] = 
    info(s"$player is the muderer")
    val room = house(location)
    murderer(goingTo = randomRooms.filter(_ != room).head)

/** 
 * The murderer's only piece of state is which room it's going to. 
 * 
 * @param gameActor A reference to the actor running the game, so we can send it our commands
 * @param goingTo The room we are going to
 */
def murderer(goingTo:Room)(using gameActor: ActorRef[GameMessage]):Behavior[Message] = Behaviors.receive { (context, msg) =>
  msg match 

    case Message.TurnUpdate(me, position, room, visiblePlayers, visibleWeapons) =>
        // Check if there is a murder available
        if visiblePlayers.size == 1 && visibleWeapons.nonEmpty then 
            gameActor ! (me, Command.Murder(visiblePlayers.head, visibleWeapons.head))

        // Check if it has reached its destination room (in which case, pick a new one)
        if room == goingTo then 
            val next = randomRooms.filter(_ != room).head
            info(s"$me is heading to $next")
            murderer(next)
        else 
            val shortest = position.shortestDirectionTo(goingTo)
            debug(s"Shortest way to $goingTo is $shortest")
            gameActor ! (me, Command.Move(shortest))
            murderer(goingTo)

    case _ => Behaviors.same


        

}