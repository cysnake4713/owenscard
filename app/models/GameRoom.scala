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
import Json.toJson
import akka.actor.ActorLogging

object GameRoom {

  implicit val timeout = Timeout(1 second)

  lazy val default = {
    val roomActor = Akka.system.actorOf(Props[GameRoom])
    roomActor
  }

  def join(username: String): Promise[(Iteratee[JsValue, _], Enumerator[JsValue])] = {
    (default ? JoinGame(username)).asPromise.map {

      case Connected(enumerator) =>

        // Create an Iteratee to consume the feed
        val iteratee = Iteratee.foreach[JsValue] { event =>
          //TODO:
          default ! GetMessage(username, event)
          //          default ! Talk(username, (event \ "text").as[String])
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

class GameRoom extends Actor with akka.actor.ActorLogging {
  import context._
  var members = Map.empty[String, Map[String, Any]]
  def receive = init

  def init: Receive = {
    case JoinGame(username) => {
      // Create an Enumerator to write to this socket
      system.log.debug("{} has join the game", username)
      val channel = Enumerator.imperative[JsValue]()
      if (members.contains(username)) {
        sender ! CannotConnect(Messages("username.used"))
      } else {
        val member = Map[String, Any](
          "channel" -> channel,
          "status" -> false)
        members = members + (username -> member)
        sender ! Connected(channel)
      }
    }

    case GetMessage(name, jsValue) => {
      (jsValue \ "kind").asOpt[String] match {
        case Some("ready") => {
          system.log.debug("{} ready to start game", name)
          members(name).updated("status", true)
          if ((true /: members) { (result, ele) => result || ele._2.get("status").asInstanceOf[Boolean] }) {
            Poker.shufflePoker()
            members.foreach(ele => {
              val pokers = Poker.dialPoker(5)
              ele._2.updated("poker", pokers)
              dial(ele._1, pokers)
            })
            become(start)
          }
        }

        case None =>
      }
    }

    case Quit(username) => {
      system.log.debug("{} exit game", username)
      members = members - username
      notifyAll("quit", username, Messages("leave.room"))
    }
  }

  def start: Receive = {
    case GetMessage(name, jsValue) => {
      system.log.debug("Start: get Message")
    }

    case Quit(username) => {
      system.log.debug("{} exit game", username)
      members = members - username
      notifyAll("quit", username, Messages("leave.room"))
    }
  }

  def dial(userName: String, poker: List[(String, Int)]) {
    val jsonObj = Json.toJson(
      poker.map(ele =>
        toJson(Map(
          "color" -> toJson(ele._1),
          "number" -> toJson(ele._2)))))
    system.log.debug("dial:generate jsonObj={}", jsonObj)
    notifyOne("dial", userName, jsonObj.toString())
  }

  def notifyAll(kind: String, user: String, text: String) {
    val msg = JsObject(
      Seq(
        "kind" -> JsString(kind),
        "user" -> JsString(user),
        "message" -> JsString(text),
        "members" -> JsArray(
          members.keySet.toList.map(JsString))))
    system.log.debug("notifyAll:send Message: {}", msg)
    members.foreach {
      case (_, ele) => ele("channel").asInstanceOf[PushEnumerator[JsValue]].push(msg)
    }
  }

  def notifyOne(kind: String, target: String, text: String) {
    val msg = JsObject(
      Seq(
        "kind" -> JsString(kind),
        "message" -> JsString(text)))
    system.log.debug("notifyOne:send message:{}", msg)
    members.foreach {
      case (key, ele) if key == target => ele("channel").asInstanceOf[PushEnumerator[JsValue]].push(msg)
    }
  }
}

case class GetMessage(userName: String, value: JsValue)
case class dial(userName: String, poker: List[(String, Int)])
case class JoinGame(userName: String)
//case class Join(username: String)
//case class Quit(username: String)
//case class Talk(username: String, text: String)
//case class NotifyJoin(username: String)
//
//case class Connected(enumerator: Enumerator[JsValue])
//case class CannotConnect(msg: String)
