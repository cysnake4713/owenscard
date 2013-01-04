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
          Logger.debug(username + " has join the game.")
          Table + new User(username, false, channel)
          Logger.debug("current table member:" + Table.members)
          MessageSender.join(username)
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
            Table.shufflePoker()
            for ((key, user) <- Table.members) {
              Logger.debug("dial poker to " + user)
              Table.dialPokerToUser(user)
              MessageSender.dial(user.name, user.pokers)
            }
            Table.setAllNotReady()
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
          if (!Table.members(name).status) {
            val switchCards = MessageReceive.parseSwitchCards(jsValue)
            Table.members(name).pokers = Table.members(name).pokers.filterNot(switchCards.contains(_))
            Table.members(name).status = true
          }
          if (Table.isReady) {
            for ((name, user) <- Table.members) {
              val switchCount = 5 - user.pokers.size
              Table.dialPokerToUser(user)
              MessageSender.switch(name, switchCount, user.pokers)
            }
            Table.setAllNotReady()
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
          if (!Table.members(name).status) {
            val showPoker = MessageReceive.parseShowCards(jsValue)
            val unShowPoker = Table.members(name).pokers.filterNot(ele => showPoker.contains(ele))
            MessageSender.show(name, showPoker, unShowPoker)
            Table.members(name).status = true
            if (Table.isReady) {
              Table.setAllNotReady()
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
          Table.members(name).status = true
          if (Table.isReady) {
            for ((name, user) <- Table.members) {
              if (user.pokers != null)
                MessageSender.openAll(name, user.pokers)
            }
            Table.setAllNotReady()
            MessageSender.finished()
            Table.clean()
            context.unbecome()
          }
        }

        case Some("fold") => {
          Table.members(name).status = true
          Table.members(name).pokers = null
          MessageSender.fold(name)
          if (Table.isReady) {
            for ((name, user) <- Table.members) {
              if (user.pokers != null)
                MessageSender.openAll(name, user.pokers)
            }
            Table.setAllNotReady()
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
    Table - userName
    context.unbecome()
  }
}


