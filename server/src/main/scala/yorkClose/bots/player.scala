package yorkClose.bots

import org.apache.pekko
import pekko.actor.typed.*
import pekko.actor.typed.scaladsl.*

import yorkClose.*
import game.*

import scala.util.Random

/** Provided as a suggestion for how to manage your state
  *
  * @param gameActor
  *   A reference to the actor running the game, so we can send it our commands
  *   \@param headingTo - the room you would like to go to
  * @param suspects
  *   \- the list of suspects not yet eliminated (either by being proven
  *   innocent or by being 'eliminated')
  * @param weaponsSeen
  *   \- where you have seen weapons, so you can track them
  * @param usedWeapons
  *   \- weapons that have disappeared
  * @param knownVictims
  *   \- victims you have heard scream
  */
case class PlayerState(
    goingTo: Room,
    suspects: Seq[Player],
    weaponsSeen: Map[Weapon, Room],
    usedWeapons: Seq[Weapon],
    knownVictims: Seq[Player],
    visiblePlayers: Set[Player]
)

object PlayerState:
  def empty = PlayerState(
    randomRooms.head,
    Player.values.toSeq,
    Map.empty,
    Seq.empty,
    Seq.empty,
    Set.empty
  )

/** You are not the murderer.
  *
  * This function needs to remain, because this is how the Game spawns the
  * players. However, upon receiving a message (e.g. the first Tick), you can
  * change to whatever handler you define
  *
  * @param gameActor
  *   A reference to the actor running the game, so we can send it our commands
  * @param player
  *   The Player this actor is playing (e.g. Player.Pink)
  * @param location
  *   The (x, y) location the player has initially spawned in
  * @return
  */
def player(p: Player, location: Location)(using gameActor: ActorRef[GameMessage]): Behavior[Message] = player(PlayerState.empty)

def player(playerState: PlayerState)(using gameActor: ActorRef[GameMessage]): Behavior[Message] = Behaviors.receive { (context, msg) =>
  msg match
    case Message.TurnUpdate(
          me, location,
          room, visiblePlayers,
          visibleWeapons) =>
    
    
    // This code makes the player randomly travel from room to room
  

    //Add any visible weapons in this room to our map seen weapons
      val seenWeapons = playerState.weaponsSeen ++ visibleWeapons
        .map(weapon => (weapon -> room))
        .toMap

      /* 
      If there is no weapon in this room check to see if we have seen one here before (may have been used before we ever came here)
      Two cases
        Yes we have seen a weapon here before -> add it to the list of used weapons because the weapon has been consumed
        No we haven't seen a weapon here before -> do nothing because we don't know what the weapon was and can't do anything with this info\
      */
      val usedWeapons =
        if (visibleWeapons.size == 0) then
          playerState.weaponsSeen.find((weapon, weaponRoom) =>
            weaponRoom == room
          ) match
            case None               => playerState.usedWeapons
            case Some(weapon, room) => playerState.usedWeapons.appended(weapon)
        else playerState.usedWeapons

      val removeDead = playerState.suspects.filterNot(player =>
        playerState.knownVictims.contains(player)
      )

      /* 
      If someone is dead they cannot be the murderer, remove them from the list of suspects
      */
      val suspects = removeDead.filterNot(player => player == me)

      /* 
      If the list of suspects is exactly 1 everyone else is either dead OR we have had them in sight while a murder occurred
      Therefore the last person must be the murderer. 
      Send a message to ElizabethDacre accusing this person
        Accuse the only person left in the suspects list
        Take the head of the used weapons list
        Take the room associated with that used weapon because we know a murder occured there. 
      */
      if suspects.size == 1 then
        gameActor ! ElizabethDacreCommand.Accuse(
          suspects.head,
          playerState.knownVictims.head,
          playerState.usedWeapons.head,
          playerState.weaponsSeen(playerState.usedWeapons.head)
        )

        /* 
        if we have arrived at our destination, set a new random destionation
        Update the playerState with all of this learned information. 
         */
      if room == playerState.goingTo then
        player(
          playerState.copy(
            goingTo = randomRooms.head,
            suspects = suspects,
            usedWeapons = usedWeapons,
            weaponsSeen = seenWeapons,
            visiblePlayers = visiblePlayers
          )
        )
      else
        //otherwise update the state with all the learned information to carry to the next tick
        //Send a message to the gameActor requesting this player be moved towards destination via shortest path
        gameActor ! (me, Command.Move(
          location.shortestDirectionTo(playerState.goingTo)
        ))
        player(
          playerState.copy(
            weaponsSeen = seenWeapons,
            suspects = suspects,
            usedWeapons = usedWeapons,
            visiblePlayers = visiblePlayers
          )
        )

        /* 
        Someone has died.
          Remove that person from the list of suspects
          Remove anyone I can visually see from the list of suspects
          If only one suspect remains, accuse them with the information I have available
          If not return new playerState with learned information
        
         */
    case Message.Scream(victim) =>
      val knownVictims = playerState.knownVictims.appended(victim)
      val removeMurdered =
        playerState.suspects.filterNot(player => knownVictims.contains(player))
      val removeSeen = removeMurdered.filterNot(player =>
        playerState.visiblePlayers.contains(player)
      )
      if removeSeen.size == 1 then
        gameActor ! ElizabethDacreCommand.Accuse(
          removeSeen.head,
          knownVictims.head,
          playerState.usedWeapons.head,
          playerState.weaponsSeen(playerState.usedWeapons.head)
        )
      player(
        playerState.copy(knownVictims = knownVictims, suspects = removeSeen)
      )

      /* 
      Do nothing, we're dead
      */
    case Message.YouHaveBeenMurdered =>
      player(playerState)

      /* 
      Do nothing, the game is won
      */
    case Message.Victory =>
      player(PlayerState.empty)


      /* 
      Hide the game is lost
      */
    case Message.Defeat =>
      player(PlayerState.empty)
    
      /* 
      For all remaining cases just do nothing. 
       */
    case _ =>
      player(playerState)

}
