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
    case JoinGame(username) => joinGame(username)

    case GetMessage(name, jsValue) => {
      (jsValue \ "kind").asOpt[String] match {
        case Some("ready") =>
          Logger.debug(name + " ready to start game")
          table.preparingMembers(name).status = true
          Logger.debug("is everyone ready? " + table.isReadyPrepare)
          if (table.isReadyPrepare) {
            table.startGame()
            table.shufflePoker()
            for (name <- table.memberOrder) {
              if (table.gamingMembers.contains(name)) {
                Logger.debug("dial poker to " + name)
                table.dialPokerToUser(table.gamingMembers(name))
                MessageSender.dial(name, table.gamingMembers(name).pokers)
              }
            }
            table.setAllNotReady()
            become(switch)
          }

        case Some("getMember") => {
          Logger.debug(name + " want getMember")
          MessageSender.sendCurrentMemberToUser(name)
        }

        case _ => Logger.debug("no match")

      }
    }

    case Quit(username) => quitGame(username)
  }

  def switch: Receive = {
    case JoinGame(username) => joinGame(username)

    case GetMessage(name, jsValue) => {
      Logger.debug("switch: get Message" + jsValue)
      (jsValue \ "kind").asOpt[String] match {
        case Some("switch") => {
          if (!table.gamingMembers(name).status) {
            val switchCards = MessageReceive.parseCards(jsValue)
            table.gamingMembers(name).pokers = table.gamingMembers(name).pokers.filterNot(switchCards.contains(_))
            table.gamingMembers(name).status = true
          }
          if (table.isReadyGaming) {
            for ((name, user) <- table.gamingMembers) {
              val switchCount = 5 - user.pokers.size
              table.dialPokerToUser(user)
              MessageSender.switch(name, switchCount, user.pokers)
            }
            table.setAllNotReady()
            become(show)
          }
        }

        case _ => Logger.debug("no match")
      }
    }

    case Quit(username) => quitGame(username)
  }

  def show: Receive = {
    case JoinGame(username) => joinGame(username)
    case GetMessage(name, jsValue) => {
      Logger.debug("show: get Message" + jsValue)
      (jsValue \ "kind").asOpt[String] match {
        case Some("show") => {
          if (!table.gamingMembers(name).status) {
            val showPoker = MessageReceive.parseCards(jsValue)
            table.gamingMembers(name).showPokers = showPoker
            val unShowPoker = table.gamingMembers(name).pokers.filterNot(ele => showPoker.contains(ele))
            MessageSender.show(name, showPoker, unShowPoker)
            table.gamingMembers(name).status = true
            if (table.isReadyGaming) {
              table.setAllNotReady()
              MessageSender.end()
              become(end)
            }
          }
        }
        case _ => Logger.debug("no match")
      }
    }
    case Quit(username) => quitGame(username)
  }

  def end: Receive = {
    case GetMessage(name, jsValue) => {
      Logger.debug("end: get Message" + jsValue)
      (jsValue \ "kind").asOpt[String] match {
        case Some("open") => {
          table.gamingMembers(name).status = true
          if (table.isReadyGaming) {
            for ((name, user) <- table.gamingMembers) {
              if (user.pokers != null)
                MessageSender.open(name, user.pokers)
            }
            table.setAllNotReady()
            MessageSender.finished()
            table.clean()
            table.endGame()
            context.unbecome()
          }
        }

        case Some("fold") => {
          table.gamingMembers(name).status = true
          table.gamingMembers(name).pokers = null
          MessageSender.fold(name)
          if (table.isReadyGaming) {
            for ((name, user) <- table.gamingMembers) {
              if (user.pokers != null)
                MessageSender.open(name, user.pokers)
            }
            table.setAllNotReady()
            MessageSender.finished()
            table.clean()
            table.endGame()
            context.unbecome()
          }
        }
        case _ => Logger.debug("no match")

      }

    }
    case Quit(username) => quitGame(username)
  }

  def quitGame(userName: String) {
    system.log.debug("{} exit game", userName)
    table - userName
    if (table.gamingMembers.isEmpty) context.unbecome()
  }

  def joinGame(username: String) {
    // Create an Enumerator to write to this socket
    val channel = Enumerator.imperative[JsValue]()
    if (table.contains(username)) sender ! CannotConnect(Messages("username.used"))
    else {
      if (table.size == 5) sender ! CannotConnect("already have 5 users")
      else {
        Logger.debug(username + " has join the game.")
        table + new User(username, false, channel)
        Logger.debug("current praparing table member:" + table.preparingMembers)
        Logger.debug("current gaming table member:" + table.gamingMembers)
        MessageSender.join(username)
        sender ! Connected(channel)
        if (table.isStarted == true) MessageSender.sync(username)

      }
    }
  }
}


