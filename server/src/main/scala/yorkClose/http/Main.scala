package yorkClose.http

import org.apache.pekko
import pekko.http.scaladsl.Http
import pekko.http.scaladsl.server.Directives.*
import pekko.http.scaladsl.model.*
import pekko.actor.typed.scaladsl.*
import pekko.actor.typed.ActorSystem

import org.apache.pekko.http.scaladsl.server.{ExceptionHandler, Route}

import pekko.stream.scaladsl.*
import pekko.stream.OverflowStrategy
import pekko.stream.CompletionStrategy

import yorkClose.{given, *}
import scala.concurrent.ExecutionContext
import spray.json.JsObject
import org.apache.pekko.http.scaladsl.model.sse.ServerSentEvent
import org.apache.pekko.actor.typed.ActorRef

import scala.concurrent.duration.DurationInt
import org.apache.pekko.actor.typed.Behavior
import yorkClose.game.GameControl


object GuardianActor {

    def apply():Behavior[String]  = {
        Behaviors.setup { (context) => 
            var gameActor = context.spawn(unstartedGame, "game")
            var i = 0

            Behaviors.receiveMessage { (msg) => msg match {
                case "start" =>
                    gameActor ! GameControl.Initialise(null)  
                    Behaviors.same

                case "restart" => 
                    context.stop(gameActor)
                    i = i + 1
                    gameActor = context.spawn(unstartedGame, "game" + i)
                    gameActor ! GameControl.Initialise(null) 
                    Behaviors.same 

                case "tick" => 
                    gameActor ! GameControl.Tick
                    Behaviors.same

            }}            
        
        }
    }
}

/**
  * The actor system is a top-level "given" instance so that it is automatically found where it
  * is needed
  */
given actorSystem:ActorSystem[String] = ActorSystem(GuardianActor(), "my-system")


// An event queue for sending live events to the client. Offer a JSON object to the queue, and it'll appear in the browser.
// This is a filthy hack. We assume there is only ever ONE browser connected to the server, so only ONE event queue is needed
// I've put it into a global variable when it's created, so things like logActor can also be global.
// Horribly dirty, but I figure teaching you to receive the actorRef as a parameter with gameActor is enough and I don't want to 
// load you down with actor parameters. This way, the log actor can be a global.
var eventQueue:Option[SourceQueueWithComplete[JsObject]] = None

/** 
 * Runs our server
 * 
 * @param port - the port to start it on, e.g. 8080
 * @param stopOnReturn - whether you want it to wait for you to press enter to stop the server, or leave it running indefinitely
 */
@main def main(port:Int, stopOnReturn:Boolean) = {
    import pekko.http.scaladsl.marshalling.sse.EventStreamMarshalling.*

    // Define the routes
    val route = Route.seal(concat(

        // So we can always test if the server is running by opening the /ping url in a browser
        path("ping") {
            get {
                complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "pong"))
            }
        },

        // Load index.html on the / route
        pathSingleSlash {
            encodeResponse {
              getFromResource("static/index.html")
            }
        },

        // The UI JavaScript, to the server, is just a file.
        // This path serves it up to the client, because our build config put the compiled JS in our resources.
        pathPrefix("assets" / Remaining) { file =>
            encodeResponse {
                getFromResource(file)
            }
        },

        // When you hit the start/restart button in the client
        path("restart") {
            post {
                info("Restart request received")
                actorSystem.tell("restart")
                complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "Restart request sent"))
            }
        },

         // When you hit the start/restart button in the client
        path("tick") {
            post {                
                actorSystem.tell("tick")
                complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "Ticked"))
            }
        },

        // This path uses the "Server Sent Events" protocol to send data to the client
        // That'll include log messages and UI updates
        path("events") {
            get {
                complete {
                    Source.queue[JsObject](bufferSize = 1000, overflowStrategy = OverflowStrategy.dropHead)
                        .mapMaterializedValue((queue) => 
                            // A filthy hack. 
                            // When the request comes in and Pekko "materializes" the source (actually creates the queue), we grab the queue and
                            // stuff it in a global variable so logActor and everyone else can see it. 
                            // Not good practice, but since we're only ever going to have one browser connected we'll put up with it for simplicity.
                            // NOTE: This means if you connect a second browser, the first will stop hearing the messages (as its queue replaces the old one!)
                            eventQueue = Some(queue)                            
                        )
                        .map(x => ServerSentEvent(x.prettyPrint))
                        .keepAlive(1.second, () => ServerSentEvent.heartbeat)
                }
            }
        }

    ))

    // We use the Execution Context (thread pool) the game started
    given ec:ExecutionContext = actorSystem.executionContext

    // Start up the server
    val server = Http().newServerAt("localhost", port).bind(route)

    // Handle stopping the server
    if (stopOnReturn) then
        println(s"Server online at http://localhost:$port/\nPress RETURN to stop...")
        scala.io.StdIn.readLine() // let it run until user presses return
        info("Shutting down")
        server
        .flatMap(_.unbind()) // trigger unbinding from the port
        .onComplete(_ => actorSystem.terminate()) // and shutdown when done
    else
        println(s"Server online at http://localhost:$port/")
        Runtime.getRuntime.addShutdownHook(new Thread() {
        override def run:Unit =
            for exited <- actorSystem.whenTerminated do System.exit(0)
            actorSystem.terminate()
        })

}