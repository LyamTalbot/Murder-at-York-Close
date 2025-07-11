package yorkClose

// This is just copied from the server. 
// It just makes our UI rendering code easier to see to have it here
// We could include a "common" package compiled to both JS and JVM if we wanted, but that'd increase
// the complexity of the project for students too much.


/**
  * The game is tile-based, so a player's position is the coordinates of the square they are standing on
  */
type Location = (Int, Int)

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


val weaponLabelLocs = {
        import Room.*
        Map(
            DiningRoom -> (1, 3),
            Lounge -> (7, 3),
            Conservatory -> (11, 3),
            Terrace -> (15, 3),
            Garden -> (18, 3),
            Kitchen -> (1, 7),
            Hall -> (8, 6),
            Pantry -> (5, 9),
            Study -> (8, 9),
            Workshop -> (11, 9),
            Boatshed -> (21, 9),
        )
    }