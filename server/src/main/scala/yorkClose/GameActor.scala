package yorkClose

import org.apache.pekko
import pekko.actor.typed.*
import pekko.actor.typed.scaladsl.*

import game.* 
import bots.*

import scala.concurrent.ExecutionContext.Implicits.global

import scala.util.Random


/**
  * The game actor is a globally visible actor.
  * 
  * It handles the game state. Every 16ms, it receives a tick message
  * and executes the actions on the tanks (and sends out their new states). In the
  * meantime, it waits for commands from the tanks to apply on the next tick.
  * 
  * Its behaviour is defined by the gameHandler function, starting out empty.
  */
// val gameActor = Behaviors.setup(actorSystem, unstartedGame) //actorSystem.spawn(unstartedGame)

/**
  * An unstarted game will only respond to an Initialise message
  *
  * @return
  */
def unstartedGame:Behavior[GameMessage] = Behaviors.receive { (context, msg) =>
  msg match 
    case GameControl.Initialise(ui) => 
      // Pick a random murderer
      val killer = Random.shuffle(Player.values).head

      // Randomise the weapon locations
      val weaponLocations = Weapon.values.zip(randomRooms).toMap

      // Set up the players
      val playerLocations = (for p <- Player.values yield p -> randomLocation()).toMap
      val playerActors = (for p <- Player.values yield
        p -> (if p == killer then 
          context.spawn(murderer(p, playerLocations(p))(using context.self), p.toString) 
        else 
          context.spawn(player(p, playerLocations(p))(using context.self), p.toString)
        )
      ).toMap

      // Create the starting game state
      val state = GameState(playerActors, playerLocations, weaponLocations, Nil)

      // Update the user interface
      sendGameStateEvent(state)

      // Become a started game
      startedGame(state, ui, Nil)

    // The game has not started!
    case _ => Behaviors.same
}

/** Sends a game state event to the UI */
def sendGameStateEvent(gs:GameState):Unit = {
  import spray.json.*
  for eq <- http.eventQueue do eq.offer(JsObject(
    "kind" -> JsString("gameState"),
    "players" -> JsObject((for (p, (x, y)) <- gs.playerLocation yield p.colour -> JsArray(JsNumber(x), JsNumber(y)))),
    "weapons" -> JsObject(for (w, r) <- gs.weaponRoom yield w.toString -> JsString(r.toString)),
    "playerRooms" -> JsObject(for (p, r) <- gs.playerRoom yield p.toString -> JsArray(JsString(p.name), JsString(r.toString))),
  ))
}

/**
 * A started game has a state, knows the UI to update, and keeps a list of commands players have asked it to perform on the next tick
 */
def startedGame(
  state:GameState, 
  ui:ActorRef[GameState],
  queued:List[(Player, Command)]
):Behavior[GameMessage] = Behaviors.receive { (context, msg) =>
  
  msg match

    // Indicates the game is ready to show on the UI. Also sent before the first tick
    case GameControl.Initialise(_) => 
      warning("Received an initialise message, but already started. Restarting")      
      startedGame(state, ui, queued)

    // An accusation has been made. It is handled immediately (not queued)
    case ElizabethDacreCommand.Accuse(mm, vv, ww, rr) =>
      info(s"The ghost of Elizabeth Dacre hears the accusation that $mm killed $vv. At least one murder used the $ww. And at least one murder took place in the $rr.")
      if state.checkAccusation(mm, vv, ww, rr) then 
        info(
          "The accusation was valid! The full list of murders was: \n" + 
          (for Murder(mm, vv, ww, rr) <- state.murders.reverse yield
            s"  $mm killed $vv with the $ww in the $rr"
          ).mkString("\n")
        )
        for p <- state.playerLocation.keySet do 
          state.playerActor(p) ! Message.Victory
        
        startedGame(state.copy(playerLocation = state.playerLocation.removed(mm)), ui, queued)

      else
        info(
          "The accusation was NOT valid! The players are doomed and the murderer slips away into the night. The full list of murders was: \n" + 
          (for Murder(mm, vv, ww, rr) <- state.murders.reverse yield
            s"  $mm killed $vv with the $ww in the $rr"
          ).mkString("\n")
        )
        for p <- state.playerLocation.keySet do 
          state.playerActor(p) ! Message.Defeat
          
        startedGame(state.copy(playerLocation = Map.empty), ui, queued)

    // When we receive a command from a player, we queue it to execute on the tick
    case (player, command) => 
      trace(s"$player $command")
      startedGame(state, ui, (player, command) :: queued)

    // A tick moves the game forward
    case GameControl.Tick =>

      //queued is a list of (player, command)
      // I implemented this foldLeft implementation instead of the previous solution which was using the scala x <- X syntax (for x in X)
      var s:GameState = queued.foldLeft(state){case (gameState, (player, command)) => command match
        case Command.Move(direction) => gameState.move(player, direction)
        case Command.Murder(victim, weapon) => {
          if gameState.canMurderHappen(player, victim, weapon) then
            val newState = gameState.murder(player, victim, weapon)
            info(s"$victim has been murdered!")
            newState.playerActor(victim) ! Message.YouHaveBeenMurdered
            newState.playerActor.filterNot((player, ref) => player == victim).map((player, ref) => ref ! Message.Scream(victim))
            newState
          else
            gameState
        }
      }
      
      // Rewrite this as a foldLeft
      // var s = state
      // for (p, command) <- queued.reverse do command match {
      //   case Command.Move(direction) => 
      //     s = s.move(p, direction)

      //   case Command.Murder(victim, weapon) => 
      //     // Check that the murderer, victim, and weapon are all in the same room
      //     if s.canMurderHappen(p, victim, weapon) then 
      //       s = s.murder(p, victim, weapon)
      //       info(s"$victim has been murdered!")
      //       s.playerActor(victim) ! Message.YouHaveBeenMurdered
      //       for remaining <- s.playerLocation.keySet do
      //         s.playerActor(remaining) ! Message.Scream(victim)

      // }
      
      for p <- s.playerLocation.keySet do 
        val room = s.playerRoom(p)
        s.playerActor(p) ! Message.TurnUpdate(p, s.playerLocation(p), room, s.playersInRoom(room) - p, s.weaponsInRoom(room))

      //
      sendGameStateEvent(s)
      startedGame(s, ui, Nil)

}

