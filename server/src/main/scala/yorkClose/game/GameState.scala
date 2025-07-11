package yorkClose.game

import org.apache.pekko
import pekko.actor.typed.*
import pekko.actor.typed.scaladsl.*

import scala.util.Random

case class Murder(murderer:Player, victim:Player, weapon:Weapon, room:Room)

/**
  * The game state is simply the players and their locations. We also keep the Recipient for each player, so we can
  * send them messages.
  *
  * @param players
  */
case class GameState(
    playerActor:Map[Player, ActorRef[Message]],
    playerLocation: Map[Player, Location],
    weaponRoom: Map[Weapon, Room],
    murders: List[Murder]
) {

  /** A derived map of which players are in which rooms */
  lazy val playerRoom:Map[Player, Room] = (for (p, l) <- playerLocation yield p -> house(l))

  /** The number of players in a room */
  def playersInRoom(r:Room):Set[Player] = playerRoom.keySet.filter(playerRoom(_) == r)

  /** The set of weaponse in a room */
  def weaponsInRoom(r:Room):Set[Weapon] = weaponRoom.keySet.filter(weaponRoom(_) == r) 

  /** Checks whether a murder is valid */
  def canMurderHappen(murderer:Player, victim:Player, weapon:Weapon):Boolean = 
    playerLocation.contains(murderer)
    && playerLocation.contains(victim)
    && weaponRoom.contains(weapon)
    && playerRoom(murderer) == playerRoom(victim)
    && playerRoom(murderer) == weaponRoom(weapon)
    && playersInRoom(playerRoom(murderer)).size == 2

  /** The state after a player is murdered */
  def murder(murderer:Player, victim:Player, weapon:Weapon):GameState = 
    GameState(
      playerActor, playerLocation.removed(victim), weaponRoom.removed(weapon), 
      Murder(murderer, victim, weapon, playerRoom(victim)) :: murders
    )

  /** 
   * Checks an accusation. Note that the answers only have to be right individually, not in combination.
   * Brown has killed Blue with the Poker and Pink with the Oar, an accusation that they killed Pink with the Poker is acceptable.
   */
  def checkAccusation(murderer:Player, victim:Player, weapon:Weapon, room:Room):Boolean = {
    murders.exists(_.murderer == murderer)
    && murders.exists(_.victim == victim)
    && murders.exists(_.weapon == weapon)
    && murders.exists(_.room == room)
  }

  /** The state after a player has moved a step */
  def move(p:Player, d:Direction):GameState = 
    if playerLocation.contains(p) then 
      val actor = playerActor(p)
      val l = playerLocation(p)
      val newLoc = l.move(d)
      GameState(
        playerActor,
        playerLocation + (p -> (if isPassable(newLoc) then newLoc else l)),
        weaponRoom,
        murders
      )
    else this
}

object GameState {
  def empty = GameState(
    Map.empty,
    Map.empty,
    Weapon.values.zip(randomRooms).toMap,
    Nil
  )
}