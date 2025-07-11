package yorkClose.game

import scala.util.Random

/**
  * The tiles in the game belong to different rooms. This defines these rooms
  *
  * @param char
  * @param passable
  * @param murderEligible
  */
enum Room(val char:Char, val passable:Boolean = true, val murderEligible:Boolean = true):
    case DiningRoom extends Room('D')
    case Conservatory extends Room('C')
    case Pantry extends Room('P')
    case Hall extends Room('h')
    case Lounge extends Room('L')
    case Study extends Room('S')
    case Kitchen extends Room('K')
    case Garden extends Room('G')
    case Terrace extends Room('T')
    case Workshop extends Room('W')
    case Boatshed extends Room('B')
    case Lake extends Room('o', false, false)
    case Door extends Room('.', true, false)
    case Wall extends Room('#', false, false)

/**
  * The map of the house is parsed from this string.
  * Note - stripMargin removes the indentation and | character from the left.
  */
val houseString = """|########################
                     |#DDD#h#LLL#CCC#TT#GGGGG#
                     |#DDD.h.LLL.CCC.TT#GGGGG#
                     |#DDD#h#LLL#CCC#TT#GGooo#
                     |##.##h######.##TT.GGooo#
                     |#KKK.hhhhhhhhh.TT#GGooo#
                     |#KKK#####.##.##TT#GGooo#
                     |#KKK.PP#SS.WWW#TT#GGGBB#
                     |#KKK#PP#SS#WWW#TT#GGGBB#
                     |########################""".stripMargin


/** A map of the house */
val house:Map[Location, Room] = {
  (for 
    (row, y) <- houseString.linesIterator.zipWithIndex
    (c, x) <- row.zipWithIndex.iterator
    tile <- Room.values.find(_.char == c)
  yield (x, y) -> tile).toMap
}

/** Max x location */
lazy val maxX = house.keySet.map(_._1).max

/** Max y location */
lazy val maxY = house.keySet.map(_._2).max

/** Whether or not a location in the house is passable, i.e. players may enter that tile */
def isPassable(location:Location):Boolean = 
  house.getOrElse(location, Room.Wall).passable

/** Produces a random ordering of the tiles. Useful for shuffling weapon locations. */
def randomRooms = Random.shuffle(Room.values.filter(_.murderEligible))

/**
  * Produces a random (x, y) location that a player can be in
  */
@scala.annotation.tailrec
def randomLocation():Location = 
  val x = Random.nextInt(maxX)
  val y = Random.nextInt(maxY)
  if isPassable((x, y)) then (x, y) else randomLocation()