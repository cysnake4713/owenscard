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
          Logger.debug("is everyone ready? " + Table.isReady())
          if (Table.isReady()) {
            Poker.shufflePoker()
            for ((key, user) <- Table.members) {
              Logger.debug("dial poker!")
              val pokers = Poker.dialPoker(5)
              user.pokers = pokers
              Message.dial(user.name, pokers)
            }
            Table.setAllNotReady()
            become(start)
          }
        }
        case Some("getMember") => {
          Logger.debug(name + " want getMember")
          Message.getMember(name)
        }

        case None => {
          Logger.debug("no match")
        }
      }
    }

    case Quit(username) => {
      Logger.debug(username + " exit game")
      Table - username
      Message.quit(username)
      context.unbecome()
    }
  }

  def start: Receive = {
    case GetMessage(name, jsValue) => {
      Logger.debug("Start: get Message" + jsValue)
      (jsValue \ "kind").asOpt[String] match {
        case Some("switch") => {
          if (!Table.members(name).status) {
            val switchPokersColor = (jsValue \ "message" \\ "color").map(ele => ele.as[String])
            val switchPokersNum = (jsValue \ "message" \\ "number").map(ele => ele.as[Int])
            val result = (switchPokersColor zip switchPokersNum).toList
            Table.members(name).pokers = Table.members(name).pokers.filterNot(ele => result.contains(ele))
            Table.members(name).status = true
          }
          if (Table.isReady()) {
            Table.members.foreach {
              case (name, user) =>
                val switchCount = 5 - user.pokers.size
                user.pokers = user.pokers ++ Poker.dialPoker(switchCount)
                user.pokers = user.pokers.sortWith((e1, e2) => e1._2 < e2._2)
                Message.switch(name, switchCount, user.pokers)
            }
            Table.setAllNotReady()
            become(show)
          }
        }

        case None =>
      }
    }

    case Quit(username) => {
      system.log.debug("{} exit game", username)
      Table - username
      context.unbecome()
    }
  }

  def show: Receive = {
    case GetMessage(name, jsValue) => {
      Logger.debug("show: get Message" + jsValue)
      (jsValue \ "kind").asOpt[String] match {
        case Some("show") => {
          if (!Table.members(name).status) {
            val showPokersColor = (jsValue \ "message" \\ "color").map(ele => ele.as[String])
            val showPokersNum = (jsValue \ "message" \\ "number").map(ele => ele.as[Int])
            val showPoker = (showPokersColor zip showPokersNum).toList
            val unShowPoker = Table.members(name).pokers.filterNot(ele => showPoker.contains(ele))
            Message.show(name, showPoker, unShowPoker)
            Table.members(name).status = true
            if (Table.isReady()) {
              Table.setAllNotReady()
              Message.toEnd()
              become(end)
            }
          }
        }
        case None =>
      }
    }
    case Quit(username) => {
      system.log.debug("{} exit game", username)
      Table - username
      context.unbecome()
    }
  }

  def end: Receive = {
    case GetMessage(name, jsValue) => {

    }
    case Quit(username) => {
      system.log.debug("{} exit game", username)
      Table - username
      context.unbecome()
    }
  }

}

object Message {

  def toEnd() {
    notifyAll("toEnd", "", "")
  }

  def show(userName: String, showPoker: List[(String, Int)], unShowPoker: List[(String, Int)]) = {
    val showPokerJs = Json.toJson(
      showPoker.map(ele =>
        toJson(Map(
          "color" -> toJson(ele._1),
          "number" -> toJson(ele._2)))))
    val unShowPokerJs = Json.toJson(
      unShowPoker.map(ele =>
        toJson(Map(
          "color" -> toJson(ele._1),
          "number" -> toJson(ele._2)))))

    Table.members.foreach {
      case (name, user) if (name == userName) =>
        val result = toJson(Map("showPoker" -> showPokerJs,
          "unShowPoker" -> unShowPokerJs))
        notifyOne("showself", name, result.toString())
      case (name, user) if (name != userName) =>
        val result = toJson(Map("username" -> toJson(userName), "showPoker" -> showPokerJs, "count" -> toJson(showPoker.size)))
        notifyOne("showother", name, result.toString())
    }
  }
  def switch(userName: String, count: Int, poker: List[(String, Int)]) {
    val jsonObj = Json.toJson(
      Map("cards" -> toJson(
        poker.map(ele =>
          toJson(Map(
            "color" -> toJson(ele._1),
            "number" -> toJson(ele._2))))),
        "count" -> toJson(count)))
    Logger.debug("dial:generate jsonObj=" + jsonObj)
    notifyOne("switchresult", userName, jsonObj.toString())
  }

  def getMember(userName: String) {
    notifyOne("members", userName, "")
  }

  def quit(userName: String) {
    notifyAll("quit", userName, "")
  }

  def join(user: String) {
    notifyAll("members", user, "")
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
    val index = Table.memberOrder.indexWhere(_ == userName)
    val memberForJS = Table.memberOrder.slice(index, Table.memberOrder.size) ::: Table.memberOrder.slice(0, index)
    val msg = JsObject(
      Seq(
        "kind" -> JsString(kind),
        "user" -> JsString(userName),
        "message" -> JsString(text),
        "members" -> JsArray(
          memberForJS.map(JsString))))
    Logger.debug("notifyOne: send message:" + msg)
    Table.members.foreach {
      case (key, user) if key == userName =>
        user.channel.push(msg)
      case _ =>
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

  def setAllNotReady() = {
    members.foreach {
      case (name, user) => user.status = false
    }
  }
}

class User(val name: String, var status: Boolean, val channel: PushEnumerator[JsValue]) {
  var pokers = List.empty[(String, Int)]

  override def toString(): String = {
    "name: " + name + " status:" + status + " channel:" + channel
  }
}

case class GetMessage(userName: String, value: JsValue)
case class dial(userName: String, poker: List[(String, Int)])
case class JoinGame(userName: String)

