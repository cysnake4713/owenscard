package models

import akka.actor._
import akka.util.duration._
import play.api.libs.json._
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import akka.pattern.ask
import play.api.Play.current
import play.api.i18n.Messages
import play.Logger
import akka.util.Timeout

object GameRoom {

  implicit val timeout = Timeout(1 second)

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
  implicit val table = new Table
  import context._

  def receive = init

  def init: Receive = {
    case JoinGame(username) => {
      // Create an Enumerator to write to this socket
      val channel = Enumerator.imperative[JsValue]()
      if (table.contains(username)) sender ! CannotConnect(Messages("username.used"))
      else {
        if (table.size == 5) sender ! CannotConnect("already have 5 users")
        else {
          Logger.debug(username + " has join the game.")
          table + new User(username, false, channel)
          Logger.debug("current table member:" + table.members)
          MessageSender.join(username)
          sender ! Connected(channel)

        }
      }
    }

    case GetMessage(name, jsValue) => {
      (jsValue \ "kind").asOpt[String] match {
        case Some("ready") =>
          Logger.debug(name + " ready to start game")
          table.members(name).status = true
          Logger.debug("is everyone ready? " + table.isReady)
          if (table.isReady) {
            table.shufflePoker()
            for ((key, user) <- table.members) {
              Logger.debug("dial poker to " + user)
              table.dialPokerToUser(user)
              MessageSender.dial(user.name, user.pokers)
            }
            table.setAllNotReady()
            become(switch)
          }

        case Some("getMember") => {
          Logger.debug(name + " want getMember")
          MessageSender.sendCurrentMemberToUser(name)
        }

        case None => {
          Logger.debug("no match")
        }
      }
    }

    case Quit(username) => quitGame(username)
  }

  def switch: Receive = {
    case GetMessage(name, jsValue) => {
      Logger.debug("Start: get Message" + jsValue)
      (jsValue \ "kind").asOpt[String] match {
        case Some("switch") => {
          if (!table.members(name).status) {
            val switchCards = MessageReceive.parseCards(jsValue)
            table.members(name).pokers = table.members(name).pokers.filterNot(switchCards.contains(_))
            table.members(name).status = true
          }
          if (table.isReady) {
            for ((name, user) <- table.members) {
              val switchCount = 5 - user.pokers.size
              table.dialPokerToUser(user)
              MessageSender.switch(name, switchCount, user.pokers)
            }
            table.setAllNotReady()
            become(show)
          }
        }

        case None =>
      }
    }

    case Quit(username) => quitGame(username)
  }

  def show: Receive = {
    case GetMessage(name, jsValue) => {
      Logger.debug("show: get Message" + jsValue)
      (jsValue \ "kind").asOpt[String] match {
        case Some("show") => {
          if (!table.members(name).status) {
            val showPoker = MessageReceive.parseCards(jsValue)
            val unShowPoker = table.members(name).pokers.filterNot(ele => showPoker.contains(ele))
            MessageSender.show(name, showPoker, unShowPoker)
            table.members(name).status = true
            if (table.isReady) {
              table.setAllNotReady()
              MessageSender.end()
              become(end)
            }
          }
        }
        case None =>
      }
    }
    case Quit(username) => quitGame(username)
  }

  def end: Receive = {
    case GetMessage(name, jsValue) => {
      Logger.debug("end: get Message" + jsValue)
      (jsValue \ "kind").asOpt[String] match {
        case Some("open") => {
          table.members(name).status = true
          if (table.isReady) {
            for ((name, user) <- table.members) {
              if (user.pokers != null)
                MessageSender.openAll(name, user.pokers)
            }
            table.setAllNotReady()
            MessageSender.finished()
            table.clean()
            context.unbecome()
          }
        }

        case Some("fold") => {
          table.members(name).status = true
          table.members(name).pokers = null
          MessageSender.fold(name)
          if (table.isReady) {
            for ((name, user) <- table.members) {
              if (user.pokers != null)
                MessageSender.openAll(name, user.pokers)
            }
            table.setAllNotReady()
            MessageSender.finished()
            context.unbecome()
          }
        }
        case None =>

      }

    }
    case Quit(username) => quitGame(username)
  }

  def quitGame(userName: String) {
    system.log.debug("{} exit game", userName)
    table - userName
    context.unbecome()
  }
}


