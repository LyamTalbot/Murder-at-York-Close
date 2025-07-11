package yorkClose.game

import yorkClose.*

import scala.util.Random

/**
  * The game is tile-based, so a player's position is the coordinates of the square they are standing on
  */
type Location = (Int, Int)

/** 
 * The different directions players can move in the game
 */
enum Direction(val delta:(Int, Int)):
    case North extends Direction(0, -1)
    case East extends Direction(1, 0)
    case South extends Direction(0, 1)
    case West extends Direction(-1, 0)


/** For convenience */
extension (l:Location) {

  /** The location one square in a direction */
  def move(direction:Direction):Location = 
    val (x, y) = l
    val (dx, dy) = direction.delta
    (x + dx, y + dy)

  /** The moves you can make from a given location */
  def availableDirections:Seq[Direction] = 
    Direction.values.filter(direction => isPassable(l.move(direction)))
    // for d <- Direction.values if isPassable(l.move(d)) yield d

  /** Used in pathfinding. Produces a map of how far each square is from the current point, emanating outward until some condition is reached */
  def floodFill(until: Location => Boolean):Map[Location, Int] = {
    
    @scala.annotation.tailrec
    def step(depth:Int, distances:Map[Location, Int]):Map[Location, Int] = 
      val d2 = distances ++ (for 
        (l, i) <- distances
        direction <- l.availableDirections
        after = l.move(direction) if !distances.contains(after)
      yield
        after -> (i + 1)
      )
      //trace(d2)

      if depth > 100 then
        info(s"Max depth reached trying to flood-fill path-find")
        d2
      else if d2.exists((l, _) => until(l)) then d2 else step(depth + 1, d2)

    step(0, Map(l -> 0))
  }

  /** Finds the closest location meeting a given condition */
  def findClosest(condition: Location => Boolean):Option[Location] = 
    if condition(l) then Some(l) else 
        val ff = floodFill(condition)
        ff.keySet.find(condition)

  /** Finds which direction to step in, to head towards a location meeting some criterion */
  def shortestDirectionToCondition(condition: Location => Boolean):Direction = {
    if condition(l) then 
        // Oops, standing still would have done
        Random.shuffle(availableDirections).head
    else
        findClosest(condition) match
            case None => 
                // Oops, can't get there
                Random.shuffle(availableDirections).head
            case Some(target) =>        
                val reverse = target.floodFill(_ == l)
                availableDirections.minBy((d) => reverse.getOrElse(l.move(d), Int.MaxValue))
  }
  
  def shortestDirectionTo(room:Room):Direction = shortestDirectionToCondition((loc:Location) => house(loc) == room)
}
