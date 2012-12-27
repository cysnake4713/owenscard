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
import play.Logger

object GameRoom {

  implicit val timeout = Timeout(1 second)
  var users = Map.empty[String, Promise[(Iteratee[JsValue, _], Enumerator[JsValue])]]

  lazy val default = {
    val roomActor = Akka.system.actorOf(Props[GameRoom])
    roomActor
  }

  def join(username: String): Promise[(Iteratee[JsValue, _], Enumerator[JsValue])] = {

    (default ? JoinGame(username)).asPromise.map {
      case Connected(enumerator) =>
        val iteratee = Iteratee.foreach[JsValue] { event =>
          default ! GetMessage(username, event)
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
  def receive = init

  def init: Receive = {
    case JoinGame(username) => {
      // Create an Enumerator to write to this socket
      val channel = Enumerator.imperative[JsValue]()
      if (Table.contains(username)) {
        sender ! CannotConnect(Messages("username.used"))
      } else {
        if (Table.size == 5) {
          sender ! CannotConnect("already have 5 users")
        } else {
          Logger.debug(username + " has join the game")
          Table + new User(username, false, channel)
          Logger.debug("current table member:" + Table.members)
          Message.join(username)
          sender ! Connected(channel)

        }
      }
    }

    case GetMessage(name, jsValue) => {
      (jsValue \ "kind").asOpt[String] match {
        case Some("ready") => {
          Logger.debug(name + " ready to start game")
          Table.members(name).status = true
          if (Table.isReady()) {
            Poker.shufflePoker()
            Table.members.foreach(ele => {
              val pokers = Poker.dialPoker(5)
              ele._2.pokers = pokers
              dial(ele._1, pokers)
            })
            become(start)
          }
        }

        case None =>
      }
    }

    case Quit(username) => {
      Logger.debug(username + " exit game")
      Table - username
      Message.quit(username)
    }
  }

  def start: Receive = {
    case GetMessage(name, jsValue) => {
      Logger.debug("Start: get Message")
    }

    case Quit(username) => {
      system.log.debug("{} exit game", username)
      Table - username
      Message.quit(username)
    }
  }

}

object Message {

  def quit(userName: String) {
    notifyAll("quit", userName, "")
  }

  def join(user: String) {
    notifyAll("join", user, "")
  }

  def dial(userName: String, poker: List[(String, Int)]) {
    val jsonObj = Json.toJson(
      poker.map(ele =>
        toJson(Map(
          "color" -> toJson(ele._1),
          "number" -> toJson(ele._2)))))
    Logger.debug("dial:generate jsonObj=" + jsonObj)
    notifyOne("dial", userName, jsonObj.toString())
  }

  def notifyOne(kind: String, userName: String, text: String) {
    val msg = JsObject(
      Seq(
        "kind" -> JsString(kind),
        "user" -> JsString(userName),
        "message" -> JsString(text)))
    Logger.debug("notifyOne:send message:" + msg)
    Table.members.foreach {
      case (key, user) if key == userName => user.channel.push(msg)
    }
  }

  def notifyAll(kind: String, user: String, text: String) {
    Table.members.foreach {
      case (userName, ele) => {
        val index = Table.memberOrder.indexWhere(_ == userName)
        val memberForJS = Table.memberOrder.slice(index, Table.memberOrder.size) ::: Table.memberOrder.slice(0, index)
        val msg = JsObject(
          Seq(
            "kind" -> JsString(kind),
            "user" -> JsString(user),
            "message" -> JsString(text),
            "members" -> JsArray(
              memberForJS.map(JsString))))
        Logger.debug("notifyAll:send Message: " + msg)
        ele.channel.push(msg)
      }
    }

  }

}

object Table {
  var members = Map.empty[String, User]
  var memberOrder: List[String] = List(null, null, null, null, null)

  def contains(userName: String): Boolean = {
    members.contains(userName)
  }

  def size(): Int = {
    members.size
  }

  def add(user: User) = {
    if (!isFull()) {
      members += (user.name -> user)
      val index = memberOrder.indexWhere(_ == null)
      memberOrder = memberOrder.slice(0, index) ::: List(user.name) ::: memberOrder.slice(index + 1, memberOrder.size)
    }
  }

  def +(user: User) = {
    add(user)
  }

  def -(userName: String) = {
    members -= userName
    val index = memberOrder.indexWhere(_ == userName)
    memberOrder = memberOrder.slice(0, index) ::: List(null) ::: memberOrder.slice(index + 1, memberOrder.size)
  }

  def isFull(): Boolean = {
    if (members.size >= 5) {
      true
    } else {
      false
    }
  }

  def isReady(): Boolean = {
    (true /: members) { (result, ele) => result && ele._2.status }
  }
}

class User(val name: String, var status: Boolean, val channel: PushEnumerator[JsValue]) {
  var pokers = List.empty[(String, Int)]
}

case class GetMessage(userName: String, value: JsValue)
case class dial(userName: String, poker: List[(String, Int)])
case class JoinGame(userName: String)

