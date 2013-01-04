package models

import akka.actor._
import akka.util.duration._
import play.api.libs.json._
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import akka.util.Timeout
import akka.pattern.ask
import play.api.Play.current
import play.api.i18n.Messages
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
        val iteratee = Iteratee.foreach[JsValue] {
          event =>
            default ! GetMessage(username, event)
        }.mapDone {
          _ =>
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
      if (Table.contains(username)) sender ! CannotConnect(Messages("username.used"))
      else {
        if (Table.size == 5) sender ! CannotConnect("already have 5 users")
        else {
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
        case Some("ready") =>
          Logger.debug(name + " ready to start game")
          Table.members(name).status = true
          Logger.debug("is everyone ready? " + Table.isReady)
          if (Table.isReady) {
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

        case Some("getMember") => {
          Logger.debug(name + " want getMember")
          Message.sendMember(name)
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
          if (Table.isReady) {
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
            if (Table.isReady) {
              Table.setAllNotReady()
              Message.end()
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
      Logger.debug("end: get Message" + jsValue)
      (jsValue \ "kind").asOpt[String] match {
        case Some("open") => {
          Table.members(name).status = true
          if (Table.isReady) {
            Table.members.foreach {
              case (name, user) if (user.pokers != null) =>
                Message.openAll(name, user.pokers)
              case _ =>
            }
            Table.setAllNotReady()
            Message.finished()
            context.unbecome()
          }
        }

        case Some("fold") => {
          Table.members(name).status = true
          Table.members(name).pokers = null
          Message.fold(name)
          if (Table.isReady) {
            Table.members.foreach {
              case (name, user) if (user.pokers != null) =>
                Message.openAll(name, user.pokers)
              case _ =>
            }
            Table.setAllNotReady()
            Message.finished()
            context.unbecome()
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
}


