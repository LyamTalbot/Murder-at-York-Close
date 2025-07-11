package yorkClose.game

/**
  * The names of our players. 
  */
enum Player(val name:String, val colour:String):
    case White extends Player("Chef Blanc", "white")
    case Black extends Player("Commodore Coles", "black")
    case Blue extends Player("Viscount Sapphire", "blue")
    case Brown extends Player("Sister Sable", "brown")
    case Yellow extends Player("Ms Daisy", "yellow")
    case Pink extends Player("Lady Blush", "pink")
    case Purple extends Player("Dame Hyacinth", "purple")
    case Green extends Player("Dr Frogsworth", "green")

/** Various deadly implements negligently left lying aroud for the use of ne'er-do-wells */
enum Weapon:
    case Spanner
    case Pitchfork
    case LetterOpener
    case Cleaver
    case CheeseWire
    case Oar
    case Pistol
    case Mallet
    case Rope
    case Knife
    case Screwdriver

