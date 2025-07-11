package yorkClose

import spray.json.*
import yorkClose.http.{given, *}

enum LogLevel:
    case Trace
    case Debug
    case Info
    case Warning
    case Error

def trace(s:String) = if LogLevel.Trace.ordinal >= printLevel.ordinal then logActor ! (LogLevel.Trace -> s)
def debug(s:String) = if LogLevel.Debug.ordinal >= printLevel.ordinal then logActor ! (LogLevel.Debug -> s)
def info(s:String) = if LogLevel.Info.ordinal >= printLevel.ordinal then logActor ! (LogLevel.Info -> s)
def warning(s:String) = if LogLevel.Warning.ordinal >= printLevel.ordinal then logActor ! (LogLevel.Warning -> s)
def error(s:String) = if LogLevel.Error.ordinal >= printLevel.ordinal then logActor ! (LogLevel.Error -> s)

val printLevel = LogLevel.Info

import org.apache.pekko
import pekko.actor.typed.*
import pekko.actor.typed.scaladsl.*

/**
  * The log actor prints everything to the terminal and tries to send it to the client if it has an event queue to send it on.
  */
val logActor = actorSystem.systemActorOf(
  Behaviors.receive[(LogLevel, String)] { (context, msg) =>
    val (level, message) = msg
    if level.ordinal >= printLevel.ordinal then 
      println(s"$level $message")

      for eq <- eventQueue do eq.offer(JsObject(
        "kind" -> JsString("log"),
        "level" -> JsString(level.toString),
        "msg" -> JsString(message)
      ))
    Behaviors.same
  }
  , "logger"
)
