package models

import akka.actor._
import akka.util.duration._
import play.api._
import play.api.libs.json._
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import akka.util.Timeout
import akka.pattern.ask
import play.api.Play.current
import play.api.i18n.Messages

object Robot {

  def apply(chatRoom: ActorRef) {

    // Create an Iteratee that log all messages to the console.
    val loggerIteratee = Iteratee.foreach[JsValue](event => Logger("robot").info(event.toString))

    implicit val timeout = Timeout(1 second)
    // Make the robot join the room
    chatRoom ? (Join("Robot")) map {
      case Connected(robotChannel) =>
        // Apply this Enumerator on the logger.
        robotChannel |>> loggerIteratee
    }

    // Make the robot talk every 30 seconds
    Akka.system.scheduler.schedule(
      30 seconds,
      30 seconds,
      chatRoom,
      Talk("Robot", "I'm still alive"))
  }

}

object ChatRoom {

  implicit val timeout = Timeout(1 second)

  lazy val default = {
    val roomActor = Akka.system.actorOf(Props[ChatRoom])

    // Create a bot user (just for fun)
    //    Robot(roomActor)

    roomActor
  }

  def join(username: String): Promise[(Iteratee[JsValue, _], Enumerator[JsValue])] = {
    (default ? Join(username)).asPromise.map {

      case Connected(enumerator) =>

        // Create an Iteratee to consume the feed
        val iteratee = Iteratee.foreach[JsValue] { event =>
          if ((event \ "kind").as[String] == "ready") {
            default ! Ready(username)
          }else{
	        default ! Talk(username, (event \ "text").as[String])
          }
        }.mapDone { _ =>
          default ! Quit(username)
        }

        (iteratee, enumerator)

      case CannotConnect(error) =>

        // Connection error

        // A finished Iteratee sending EOF
        val iteratee = Done[JsValue, Unit]((), Input.EOF)

        // Send an error and close the socket
        val enumerator = Enumerator[JsValue](JsObject(Seq("error" -> JsString(error)))).andThen(Enumerator.enumInput(Input.EOF))

        (iteratee, enumerator)

    }

  }

}

class ChatRoom extends Actor with akka.actor.ActorLogging {
  import context._

  var members = Map.empty[String, PushEnumerator[JsValue]]
  var membersStatus = Map.empty[String, Boolean]
  def receive = {

    case Join(username) => {
      // Create an Enumerator to write to this socket
      val channel = Enumerator.imperative[JsValue](onStart = self ! NotifyJoin(username))
      if (members.contains(username)) {
        sender ! CannotConnect(Messages("username.used"))
      } else {
        members = members + (username -> channel)
        membersStatus += (username -> false)
        sender ! Connected(channel)
      }
    }

    case NotifyJoin(username) => {
      notifyAll("join", username, Messages("join.room"))
    }

    case Talk(username, text) => {
      notifyAll("talk", username, text)
    }

    case Quit(username) => {
      members = members - username
      membersStatus = membersStatus - username
      notifyAll("quit", username, Messages("leave.room"))
    }

    case Ready(username) => {
      membersStatus = membersStatus.updated(username, true)
      system.log.debug("chatRoom:ready status {}", membersStatus)
      if ((true /: membersStatus) { _ && _._2 }) {
        notifyAll("go", "", "");
      }
    }

  }

  def notifyAll(kind: String, user: String, text: String) {
    val msg = JsObject(
      Seq(
        "kind" -> JsString(kind),
        "user" -> JsString(user),
        "message" -> JsString(text),
        "members" -> JsArray(
          members.keySet.toList.map(JsString))))
    members.foreach {
      case (_, channel) => channel.push(msg)
    }
  }

}

case class Ready(username: String)
case class Join(username: String)
case class Quit(username: String)
case class Talk(username: String, text: String)
case class NotifyJoin(username: String)

case class Connected(enumerator: Enumerator[JsValue])
case class CannotConnect(msg: String)
