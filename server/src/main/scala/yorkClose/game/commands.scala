package yorkClose.game

/**
  * Commands that players can send in the game
  */
enum Command:
    case Move(direction:Direction)
    case Murder(player:Player, weapon:Weapon)

/** 
  * Commands that can be invoked on the ghost of Elizabeth Dacre
  */
enum ElizabethDacreCommand:
    /** 
      * Note that players only need to be able to identify the murderer, any victim, any weapon, and any location.
      * They do not need to know which player was killed with which weapon or in which location. Any of the ones used will do.
      */
    case Accuse(player:Player, victim:Player, weapon:Weapon, room:Room)

