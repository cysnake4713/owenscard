package models

import play.api.libs.json.JsValue
import play.api.libs.iteratee.Enumerator

/**
 * This code is writed by matt.cai and if you want use it, feel free!
 * User: matt.cai
 * Date: 1/3/13
 * Time: 11:03 AM
 * if you have problem here, please contact me: cysnake4713@gmail.com
 */
case class GetMessage(userName: String, value: JsValue)
case class dial(userName: String, poker: List[(String, Int)])
case class JoinGame(userName: String)

case class Ready(username: String)
case class Join(username: String)
case class Quit(username: String)
case class Talk(username: String, text: String)
case class NotifyJoin(username: String)

case class Connected(enumerator: Enumerator[JsValue])
case class CannotConnect(msg: String)
