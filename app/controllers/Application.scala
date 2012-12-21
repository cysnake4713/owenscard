package controllers

import play.api._
import play.api.mvc._

import play.api.libs.json._
import play.api.libs.iteratee._

import models._

import akka.actor._
import akka.util.duration._

object Application extends Controller {

  /**
   * Just display the home page.
   */
  def index = Action { implicit request =>
    Ok(views.html.index())
  }

  /**
   * Display the chat room page.
   */
  def chatRoom(username: Option[String]) = Action { implicit request =>
    username.filterNot(_.isEmpty).map { username =>
      Ok(views.html.chatRoom(username))
    }.getOrElse {
      Redirect(routes.Application.index).flashing(
        "error" -> "Please choose a valid username.")
    }
  }

  /**
   * Handles the chat websocket.
   */
  def chat(username: String) = WebSocket.async[JsValue] { request =>

    ChatRoom.join(username)

  }

  def playRoom(userName: Option[String]) = Action { implicit request =>
    userName.filterNot(_.isEmpty).map { userName =>
      Ok(views.html.test(userName))
    }.getOrElse {
      Redirect(routes.Application.index).flashing(
        "error" -> "当前用户名不可用.")
    }
  }

  def gameOn(userName: String) = WebSocket.async[JsValue] { request =>
    GameRoom.join(userName)

  }

}
