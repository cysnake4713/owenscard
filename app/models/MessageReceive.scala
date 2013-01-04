package models

import play.api.libs.json.JsValue

/**
 * This code is written by matt.cai and if you want use it, feel free!
 * User: matt.cai
 * Date: 1/4/13
 * Time: 11:19 AM
 * if you have problem here, please contact me: cysnake4713@gmail.com
 */
object MessageReceive {
  def parseCards(jsValue: JsValue): List[(String, Int)] = {
    val pokersColor = (jsValue \ "message" \\ "color").map(_.as[String])
    val pokersNum = (jsValue \ "message" \\ "number").map(_.as[Int])
    (pokersColor zip pokersNum).toList
  }

}
