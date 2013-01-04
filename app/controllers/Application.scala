package controllers

import play.api.mvc._

import play.api.libs.json._

import models._
import play.api.Logger


object Application extends Controller {

  /**
   * Just display the home page.
   */
  def index = Action {
    implicit request =>
      Ok(views.html.index())
  }

  /**
   * Display the chat room page.
   */
  def chatRoom(username: Option[String]) = Action {
    implicit request =>
      username.filterNot(_.isEmpty).map {
        username =>
          Ok(views.html.chatRoom(username))
      }.getOrElse {
        Redirect(routes.Application.index()).flashing(
          "error" -> "Please choose a valid username.")
      }
  }

  /**
   * Handles the chat websocket.
   */
  def chat(username: String) = WebSocket.async[JsValue] {
    request =>
      ChatRoom.join(username)
  }

  def playRoom(userName: String) = Action {
    implicit request =>
      Ok(views.html.poker(userName))
  }

  def gameOn(userName: String) = WebSocket.async[JsValue] {
    Logger.debug(userName + " is connect in!")
    request =>
      GameRoom.join(userName)

  }

}
