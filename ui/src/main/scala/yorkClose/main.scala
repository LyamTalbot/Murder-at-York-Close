package yorkClose

import com.wbillingsley.veautiful.*
import scala.scalajs.js.JSON

import org.scalajs.dom
import scala.collection.mutable.Queue


// Where everyone is


given styles:html.StyleSuite = html.StyleSuite()


/** Sends a tick POST request to the server */
def sendTick() = dom.fetch("tick", new dom.RequestInit { method = dom.HttpMethod.POST })

def ui = {
    import html.* 
    div(
        h2("Murder at York Close"),
        p("""
        This page connects to your game over Server Sent Events. 
        """),
        HouseRenderer,

        p(
            button("Start/Restart", ^.onClick --> {
                dom.fetch("restart", new dom.RequestInit { method = dom.HttpMethod.POST })
            }),
            button("Tick once", ^.onClick --> sendTick()),
            TimerButton
        ),
        

        Log
    )
}

case class GameState(
    playerPositions: Map[String, Location] = Map.empty,
    weapons: Map[String, Room] = Map.empty,
    playerRooms: Map[String, (String, Room)] = Map.empty
)


object HouseRenderer extends html.DHtmlComponent {

    val gameState = stateVariable(GameState())

    val houseStyle = html.Styling(
        ""
    ).modifiedBy(        
        " .DiningRoom" -> "fill: tan;",
        " .Conservatory" -> "fill: oldlace;",
        " .Pantry" -> "fill: papayawhip;",
        " .Hall" -> "fill: lavenderblush;",
        " .Lounge" -> "fill: lavender;",
        " .Study" -> "fill: ghostwhite;",
        " .Kitchen" -> "fill: lightgoldenrodyellow;",
        " .Garden" -> "fill: lightgreen;",
        " .Terrace" -> "fill: linen;",
        " .Workshop" -> "fill: lightblue;",
        " .Boatshed" -> "fill: goldenrod;",
        " .Lake" -> "fill: cornflowerblue;",
        " .Door" -> "fill: lightgray;",
        " .Wall" -> "fill: darkgrey;",

        " .Player" -> "stroke: black; transition: cx .25s, cy .25s",
        " .Player.purple" -> "fill: purple;",
        " .Player.blue" -> "fill: blue;",
        " .Player.green" -> "fill: green;",
        " .Player.black" -> "fill: black;",
        " .Player.yellow" -> "fill: yellow;",
        " .Player.brown" -> "fill: brown;",
        " .Player.pink" -> "fill: pink;",
        " .Player.white" -> "fill: white;",

    ).register()

    val squareSize = 32
    val w = squareSize * (maxX + 1)
    val h = squareSize * (maxY + 1)

    override def render = 
        import svg.*

        html.div(

            html.<.svg(^.cls := houseStyle, ^.attr.width := w, ^.attr.height := h,

                (for (x, y) <- house.keySet.toSeq yield 
                    val sq = house((x, y))
                    rect(
                        ^.cls := ("Square", sq.toString),
                        ^.attr.x := x * squareSize,
                        ^.attr.y := y * squareSize,
                        ^.attr.width := squareSize, ^.attr.height := squareSize, 
                    )
                ),

                (for (p, (x, y)) <- gameState.value.playerPositions.toSeq yield
                    circle(
                        ^.cls := ("Player", p),
                        ^.key := p, 
                        ^.attr.r := 10, ^.attr.cx := (x * squareSize + squareSize / 2), ^.attr.cy := (y * squareSize + squareSize / 2)
                    )
                ),

                (for (w, r) <- gameState.value.weapons.toSeq yield
                    val loc = weaponLabelLocs(r)
                    text(
                        ^.cls := ("weapon"),
                        ^.key := w, 
                        ^.attr.x := loc._1 * squareSize, ^.attr.y := loc._2 * squareSize,
                        w
                    )
                ),
            ),

            html.table(^.style := "display: inline-block; vertical-align: top;",
                for (p, (n, r)) <- gameState.value.playerRooms.toSeq.sortBy(_._1) yield 
                    html.tr(
                        html.td(n),
                        html.td(r.toString())
                    )
            )

        )


}

object Log extends html.DHtmlComponent {

    private val _log = stateVariable(Queue.empty[(String, String)])

    def log(level:String, msg:String) = {
        _log.value = _log.value.enqueue((level, msg)).takeRight(1000)
    }

    def render = {
        import html.*
        div(
            h3("Log"),
            pre(^.style := "height: 400px; overflow-y: scroll; display: flex; flex-direction: column-reverse; border: 1px solid lightgray;",            
                for (lvl, msg) <- _log.value.reverse yield
                    <.p(^.cls := lvl, lvl, ": ", msg)
            )
        )
    }

}


object TimerButton extends html.DHtmlComponent {

    val running = stateVariable(false)

    dom.window.setInterval(() => { 
        if running.value then sendTick()
    }, 500)


    override def render = html.span(
        if running.value then 
            html.button("Stop", html.^.onClick --> { running.value = false })
        else 
            html.button("Play", html.^.onClick --> { running.value = true } )
    )

}


@main def main() = {
    

    println("Loaded JS")
    styles.install()
    html.mount("#render-here", ui)
    println("Mounted")


    val eventSource = org.scalajs.dom.EventSource("events")
    eventSource.onmessage = { (event) => 
        val text = event.data.toString

        if (text.nonEmpty) then 
            org.scalajs.dom.console.log(text)
            val data = JSON.parse(text).asInstanceOf[scala.scalajs.js.Dynamic]

            data.kind.toString match {
                case "log" => 
                    Log.log(data.level.toString, data.msg.toString)

                case "gameState" =>
                    import scalajs.js

                    val players = data.players.asInstanceOf[js.Dictionary[js.Array[Int]]]
                    val positions = for (p, xy) <- players yield 
                        p -> (xy(0), xy(1))

                    val weapons = data.weapons.asInstanceOf[js.Dictionary[String]]

                    val playerRooms = data.playerRooms.asInstanceOf[js.Dictionary[js.Array[String]]]

                    HouseRenderer.gameState.value = GameState(
                        playerPositions = positions.toMap,
                        weapons = (for (w, r) <- weapons yield (w, Room.valueOf(r))).toMap,
                        playerRooms = (for (p, ab) <- playerRooms yield p -> (ab(0), Room.valueOf(ab(1)))).toMap                
                    )
            }

            
    }

}